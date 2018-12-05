module Samples.Article exposing (source)

source : String
source = """
# Article with HTML & CSS

## template

```html
<article>
    <h2>History and etymology</h2>
    <p>Crumpets have been variously described as originating in <a href="/wiki/Wales" title="Wales">Wales</a><sup id="cite_ref-shulman_2-0" class="reference"><a href="#cite_note-shulman-2">[2]</a></sup> or as part of the <a href="/wiki/Anglo-Saxon" class="mw-redirect" title="Anglo-Saxon">Anglo-Saxon</a> diet,<sup id="cite_ref-hagen_3-0" class="reference"><a href="#cite_note-hagen-3">[3]</a></sup> based on proposed etymologies of the word. In either case breads were, historically, commonly cooked on a griddle whererever <a href="/wiki/Masonry_oven" title="Masonry oven">bread ovens</a> were not available. The <i>bara-planc</i>, or griddle bread, baked on an iron plate over a fire, was part of the everyday diet in Wales until the 19th century.<sup id="cite_ref-nq_4-0" class="reference"><a href="#cite_note-nq-4">[4]</a></sup> Small, oval cakes baked in this manner were called <i>picklets</i>,<sup id="cite_ref-nq_4-1" class="reference"><a href="#cite_note-nq-4">[4]</a></sup> a name used for the first recognisable crumpet-type recipe, published in 1769 by <a href="/wiki/Elizabeth_Raffald" title="Elizabeth Raffald">Elizabeth Raffald</a> in <i><a href="/wiki/The_Experienced_English_Housekeeper" title="The Experienced English Housekeeper">The Experienced English Housekeeper</a></i>.<sup id="cite_ref-davidson_5-0" class="reference"><a href="#cite_note-davidson-5">[5]</a></sup> This name was derived from the <a href="/wiki/Welsh_language" title="Welsh language">Welsh</a> <i>bara pyglyd</i> or "pitchy [i.e. dark or sticky] bread", later shortened simply to <i>pyglyd</i>;<sup id="cite_ref-edwards_6-0" class="reference"><a href="#cite_note-edwards-6">[6]</a></sup><sup id="cite_ref-luard_7-0" class="reference"><a href="#cite_note-luard-7">[7]</a></sup> the name was of some antiquity as the early 17th century lexicographer, <a href="/wiki/Randle_Cotgrave" title="Randle Cotgrave">Randle Cotgrave</a>, spoke of "our Welsh barrapycleds".<sup id="cite_ref-eds_8-0" class="reference"><a href="#cite_note-eds-8">[8]</a></sup>
    </p>
    <p>The word spread initially to the West Midlands of England, where it became anglicised as <i>pikelet</i>,<sup id="cite_ref-wilson_9-0" class="reference"><a href="#cite_note-wilson-9">[9]</a></sup> and subsequently to Cheshire, Lancashire, Yorkshire, and other areas of the north; crumpets are still referred to as <i>pikelets</i> in some areas. The word <i>crumpet</i> itself, of unclear origin, first appears in relatively modern times; it has been suggested as referring to a crumpled or curled-up cake, based on an isolated 14th century reference to a "crompid cake",<sup id="cite_ref-Ayto2012_10-0" class="reference"><a href="#cite_note-Ayto2012-10">[10]</a></sup> and the Old English word <i>crompeht</i> ("crumpled") being used to gloss Latin <i>folialis</i>, possibly a type of thin bread.<sup id="cite_ref-hagen_3-1" class="reference"><a href="#cite_note-hagen-3">[3]</a></sup> Alternatively, <i>crumpet</i> may be related to the <a href="/wiki/Wales" title="Wales">Welsh</a> <i><a href="/wiki/Crempog" title="Crempog">crempog</a></i> or <i>crempot</i>, a type of <a href="/wiki/Pancake" title="Pancake">pancake</a>.<sup id="cite_ref-shulman_2-1" class="reference"><a href="#cite_note-shulman-2">[2]</a></sup> An etymology from the <a href="/wiki/French_language" title="French language">French language</a> term <i>crompâte</i>, meaning "a paste of fine flour, slightly baked"<sup id="cite_ref-nq2_11-0" class="reference"><a href="#cite_note-nq2-11">[11]</a></sup> has also been suggested. However, a correspondent to Manchester <i>Notes and Queries</i>, writing in 1883, claimed that the <i>crampet</i>, as it was locally then known, simply took its name from the metal ring or "cramp" used to retain the batter during cooking.<sup id="cite_ref-mnq_12-0" class="reference"><a href="#cite_note-mnq-12">[12]</a></sup>
    </p>
    <p>The early crumpets were hard <a href="/wiki/Pancake" title="Pancake">pancakes</a> cooked on a griddle, rather than the soft and spongy crumpets of the <a href="/wiki/Victorian_era" title="Victorian era">Victorian era</a>, which were made with yeast.<sup id="cite_ref-Ayto2012_10-1" class="reference"><a href="#cite_note-Ayto2012-10">[10]</a></sup>  From the 19th century a little <a href="/wiki/Bicarbonate_of_soda" class="mw-redirect" title="Bicarbonate of soda">bicarbonate of soda</a> was also usually added to the batter.<sup id="cite_ref-davidson_5-1" class="reference"><a href="#cite_note-davidson-5">[5]</a></sup> In modern times, the mass production of crumpets by large commercial bakeries has eroded some regional differences. As late as the 1950s <a href="/wiki/Dorothy_Hartley" title="Dorothy Hartley">Dorothy Hartley</a> noted a wide degree of regional variation, identifying the small, thick, spongy type of crumpet specifically with the Midlands.<sup id="cite_ref-davidson_5-2" class="reference"><a href="#cite_note-davidson-5">[5]</a></sup>
    </p>
</article>
```

## style

```css
html {
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Helvetica, Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol";
  font-size: 20px;
  /* Mobile friendly: scales from 17.6px – 23px */
  /*
  font-size: calc(16px + 0.5vw);
  */
  line-height: 1.4;
  color: black;
}
article {
  max-width: 36em;
  margin: auto;
  padding-left: 3vw;
  padding-right: 3vw;
}
article a {
  color: #1F6085; /* Color from http://colormind.io/ */
  text-decoration: none;
}
article a:hover {
  text-decoration: underline;
}
article h2 {
  font-size: 1.5rem;
  font-weight: bold;
}
article sup {
  font-size: 45%;
  font-weight: bold;
  letter-spacing: 0.25px;
}
```
"""