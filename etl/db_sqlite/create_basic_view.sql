/* 
create a basic view that can be queried from R
*/

-- DROP VIEW view_gamma;

CREATE VIEW view_gamma AS 
SELECT 
replace(product, '<U+200B>', '') AS product, 
length(replace(product, '<U+200B>', '')) - 
    length(replace(replace(product, '<U+200B>', ''), 
    '+', '')) + 1 AS products_in_package, 
regular_price, 
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
discount_price, 
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
url_page, 
cast(replace(url_page, 'p_', '') AS INT) AS url_page_num, 
datetime(crawled_runtime) AS crawled_runtime, 
strftime('%Y %m %d', crawled_runtime) AS crawled_date, 
crawled_url
FROM tbl_alpha;

/* 

SELECT 
regular_price_val
FROM view_gamma 
LIMIT 10;

SELECT 
product, 
length(replace(product, '<U+200B>', '')) - 
    length(replace(replace(product, '<U+200B>', ''), 
    '+', '')) + 1 AS interim
FROM tbl_alpha 
LIMIT 50;

*/
