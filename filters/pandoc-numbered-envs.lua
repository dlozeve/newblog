-- pandoc-numbered-envs.lua
--
-- Automatically number theorem-like environments (definition, proposition,
-- theorem, …) and resolve cross-references to them, a bit like \label/\ref in
-- LaTeX.
--
-- To number an environment and make it referenceable, give it an id with
-- #+attr_html just before the block:
--
--   #+attr_html: :id def-topology
--   #+begin_definition
--   ...
--   #+end_definition
--
-- Every environment of a numbered kind gets a number, whether or not it has an
-- id; only blocks with an id can be referenced.  Unnumbered environments such
-- as proof still get their "Proof." label.  To reference one, use an ordinary
-- org link:
--
--   See [[def-topology]] and [[def-topology][the definition of a topology]].
--
-- A bare reference is filled in with the name and number ("Definition 1"),
-- while a reference with its own description keeps that description.  Both are
-- turned into links pointing at the environment.
--
-- Note: prefer hyphenated ids (def-topology).  Ids containing a colon
-- (def:topology) are also supported, but pandoc parses them as ordinary links
-- rather than fuzzy links, which is handled here too.

-- Environment classes that are numbered, mapped to their printed name.
local environments = {
  definition  = "Definition",
  proposition = "Proposition",
  theorem     = "Theorem",
  lemma       = "Lemma",
  corollary   = "Corollary",
  claim       = "Claim",
  remark      = "Remark",
  example     = "Example",
  exercise    = "Exercise",
}

-- Environment classes that get a label but no number, mapped to that label.
local unnumbered = {
  proof = "Proof",
}

local counters = {}
local labels = {}

-- Return the first class of a div that appears in the given table, or nil.
local function class_in(div, table)
  for _, class in ipairs(div.classes) do
    if table[class] then return class end
  end
  return nil
end

-- Prepend the given inlines, wrapped in a label span, to the div's first
-- paragraph (or as a new paragraph when the div does not start with one).
local function prepend_label(div, inlines)
  local prefix = { pandoc.Span(inlines, { class = "env-label" }), pandoc.Space() }
  local first = div.content[1]
  if first and first.t == "Para" then
    for i = #prefix, 1, -1 do
      table.insert(first.content, 1, prefix[i])
    end
  else
    table.insert(div.content, 1, pandoc.Para(prefix))
  end
end

-- Number a single environment div, register its label, and prepend the
-- "Name N." prefix to its first paragraph.  Unnumbered environments (proof)
-- just get their name as a prefix.
local function label_environment(div)
  local env = class_in(div, environments)
  if env then
    counters[env] = (counters[env] or 0) + 1
    local number = counters[env]
    local name = environments[env]

    if div.identifier ~= "" then
      labels[div.identifier] = { name = name, number = number }
    end

    prepend_label(div, { pandoc.Str(name), pandoc.Space(), pandoc.Str(tostring(number) .. ".") })
    return div
  end

  local proof = class_in(div, unnumbered)
  if proof then
    prepend_label(div, { pandoc.Str(unnumbered[proof] .. ".") })
    return div
  end

  return nil
end

-- Look up the label registered for a reference target, with or without a
-- leading "#".
local function lookup(target)
  if not target or target == "" then return nil end
  if labels[target] then return labels[target] end
  if target:sub(1, 1) == "#" and labels[target:sub(2)] then
    return labels[target:sub(2)]
  end
  return nil
end

-- True when pandoc generated the link text itself (i.e. the author did not
-- provide a description), so that we may replace it with "Name N".
local function is_automatic_text(text, target)
  return text == target or text == "#" .. target
end

local function reference_text(content, target, info)
  if is_automatic_text(pandoc.utils.stringify(content), target) then
    return { pandoc.Str(info.name), pandoc.Space(), pandoc.Str(tostring(info.number)) }
  end
  return nil
end

-- Fuzzy org links ([[def-topology]]) come out of the org reader as spans with
-- class "spurious-link" and a "target" attribute.
local function resolve_span(span)
  local target = span.attributes["target"]
  local info = lookup(target)
  if not info then return nil end
  local text = reference_text(span.content, target, info)
  return pandoc.Link(text or span.content, "#" .. target:gsub("^#", ""))
end

-- Links with a colon in the target ([[def:topology]]) are parsed as real links.
local function resolve_link(link)
  local info = lookup(link.target)
  if not info then return nil end
  local text = reference_text(link.content, link.target, info)
  if text then link.content = text end
  link.target = "#" .. link.target:gsub("^#", "")
  return link
end

function Pandoc(doc)
  counters = {}
  labels = {}
  -- Two passes: collect and number every environment first, so that forward
  -- references are resolved too, then rewrite the references.
  doc = doc:walk({ Div = label_environment })
  doc = doc:walk({ Span = resolve_span, Link = resolve_link })
  return doc
end
