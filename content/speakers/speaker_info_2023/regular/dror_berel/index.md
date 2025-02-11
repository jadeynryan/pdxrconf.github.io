---
title: "Dror Berel"
url: "2023/regular/dror_berel"
---

### Tidy everything… How I finally got to dive in Time series, Tree and Graph/Network data structures and analysis, thanks to their tidy packages

For years I was trying to learn and use R data structures such as xts for time series, dendograms for trees, and graphs from the igraph package. Perhaps what made it difficult and less intuitive was that there was always some piece of the data structure hidden in the class, or not printed in the default abstraction of the object/class and its projections. This was finally clearly visible with the tidy approach, that defines a tidy tabular structures for the different components, and enforce a cohesive system around it to ensure the more complex stuff is properly handled behind the scenes.
In this talk I will review some examples: the tsibble object from the tidyverts ecosystem, the treedata object from the tidytree ecosystem, and the tbl_graph object from the tidytgraph package. I will also demonstrate how I leveraged tibble’s nested structure to embed S4 objects into columns, and systematically operate on them with a purrr (row-wise) manner.

<br><br>

<table>
  <tr><td><img width="300px" style="float: left; padding: 0px 20px 0px 0px;" 
           src="../../../../img/speakers/speakers_2023/dror_berel.jpg" alt="Dror Berel headshot"></td>
  <td>
      <h5>Pronouns: he/him</h5>
      <h5>Seattle, WA, USA</h5>
      Dror Berel is a statistician with over 20 years of work experience in both academia and industry. He loves using R for (almost) everything. One time he even drew a heart with his spouse's name for Valentine's day, using R of course. He works as a consultant, solving business problems and scale analytical tools for diverse data domains, leveraging both traditional Machine learning and Causal Inference along with modern approaches.
      </td></tr>

</table>


