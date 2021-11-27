# ski_shop
personal-use web crawling project/practice

notes:
- utilize the rvest tidyverse package to crawl and compile data from a website
- structure = crawler runs and reads the html, then outputs a csv of the parsed data to a csv in the ore folder, then create a sqlite db to house the crawled results, pre-process the data in the database with a view, then query that view and analyze and visualize in an r script; 

Confirming that it is ok to review the shop portion of the site by confirming it is not in the robots.txt file disallows:
https://www.evo.com/robots.txt
User-agent: *
Disallow: /bin/
Disallow: /css/
Disallow: /checkout/thankyou
Disallow: /kbi/
Disallow: /less/
Disallow: /outlet/product/
Disallow: /retail/product/
Disallow: /scripts/
Disallow: /svg/
Disallow: /views/
Disallow: /xml/

