/* :::::::::::::::::::::::::::::::::::::::::::::::::::::::::
create a basic view that can be queried from R
::::::::::::::::::::::::::::::::::::::::::::::::::::::::: */

-- DROP VIEW view_gamma;

CREATE VIEW view_gamma AS 
SELECT 
replace(product, '<U+200B>', '') AS product, 
length(replace(product, '<U+200B>', '')) - 
    length(replace(replace(product, '<U+200B>', ''), 
    '+', '')) + 1 AS products_in_package, 
--regular_price, 
IIF(instr(regular_price, ' - ') > 0, 
    TRUE, FALSE) AS has_price_range_ind, 
cast(replace(replace(IIF(IIF(instr(regular_price, ' - ') > 0, 
                TRUE, FALSE) = 1, 
        substr(regular_price, 1, instr(regular_price, ' - ')), 
        regular_price), '$', ''), 
        ',', '') AS NUMERIC) AS regular_price_val, 
cast(replace(replace(IIF(IIF(instr(regular_price, ' - ') > 0, 
                TRUE, FALSE) = 1, 
        substr(regular_price, instr(regular_price, ' - ') + 3, 
                length(regular_price)), 
        regular_price), '$', ''), 
        ',', '') AS NUMERIC) AS regular_price_val_toprange, 
--discount_price, 
cast(replace(replace(IIF(IIF(instr(
    replace(replace(replace(discount_price, ' Limited Time', ''), ' Sale', ''), 'Outlet: ', ''), 
        ' - ') > 0, TRUE, FALSE) = 1, 
    substr(
        replace(replace(replace(discount_price, ' Limited Time', ''), ' Sale', ''), 'Outlet: ', ''), 
        1, instr(
            replace(replace(replace(discount_price, ' Limited Time', ''), ' Sale', ''), 'Outlet: ', ''), 
            ' - ')), 
        replace(replace(replace(discount_price, ' Limited Time', ''), ' Sale', ''), 'Outlet: ', '')), 
            '$', ''), ',', '') AS NUMERIC) AS discount_price_val, 
--url_page, 
cast(replace(url_page, 'p_', '') AS INT) AS url_page_num, 
datetime(crawled_runtime) AS crawled_runtime, 
strftime('%Y-%m-%d', crawled_runtime) AS crawled_date
--crawled_url
FROM tbl_alpha;

/* create a view that just gives a summary of all crawl runs */

-- DROP VIEW view_meta1;

CREATE VIEW view_meta1 AS 
SELECT 
crawled_runtime, 
COUNT(crawled_runtime)
FROM tbl_alpha 
GROUP BY crawled_runtime
ORDER BY crawled_runtime DESC; 

/* create a view off the gamme view with more cleaning/cols */

-- DROP VIEW view_ultra;

CREATE VIEW view_ultra AS 
SELECT 
*, 
(discount_price_val - regular_price_val) AS delta_price_nominal, 
(ROUND(discount_price_val, 2) / regular_price_val) AS delta_price_percent, 
IIF(crawled_runtime = (SELECT crawled_runtime 
                        FROM view_meta1 LIMIT 1), 
    TRUE, FALSE) AS latest_crawl_run_ind
FROM view_gamma;


/* tests ?????????????????????????????????????????????????

SELECT 
regular_price_val
FROM view_gamma 
LIMIT 10;

SELECT
regular_price_val, 
discount_price_val, 
delta_price_nominal, 
latest_crawl_run_ind
FROM view_ultra 
ORDER BY crawled_runtime DESC 
LIMIT 10; 

SELECT 
IIF(instr(regular_price, ' - ') > 0, 
    TRUE, FALSE) AS has_price_range_ind, 
cast(replace(replace(IIF(IIF(instr(regular_price, ' - ') > 0, 
                TRUE, FALSE) = 1, 
        substr(regular_price, 1, instr(regular_price, ' - ')), 
        regular_price), '$', ''), 
        ',', '') AS NUMERIC) AS regular_price_val, 
cast(replace(replace(IIF(IIF(instr(regular_price, ' - ') > 0, 
                TRUE, FALSE) = 1, 
        substr(regular_price, instr(regular_price, ' - ') + 3, 
                length(regular_price)), 
        regular_price), '$', ''), 
        ',', '') AS NUMERIC) AS regular_price_val_toprange, 
cast(replace(replace(IIF(IIF(instr(
    replace(replace(replace(discount_price, ' Limited Time', ''), ' Sale', ''), 'Outlet: ', ''), 
        ' - ') > 0, TRUE, FALSE) = 1, 
    substr(
        replace(replace(replace(discount_price, ' Limited Time', ''), ' Sale', ''), 'Outlet: ', ''), 
        1, instr(
            replace(replace(replace(discount_price, ' Limited Time', ''), ' Sale', ''), 'Outlet: ', ''), 
            ' - ')), 
        replace(replace(replace(discount_price, ' Limited Time', ''), ' Sale', ''), 'Outlet: ', '')), 
            '$', ''), ',', '') AS NUMERIC) AS discount_price_val, 
discount_price_val - regular_price_val AS delta_price_nominal
FROM tbl_alpha 
LIMIT 10;

*/
