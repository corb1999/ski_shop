/* 
create a basic view that can be queried from R
*/

CREATE VIEW view_gamma AS 
--SELECT * FROM tbl_alpha;
SELECT 
product, 
regular_price, 
substr(regular_price, '-', 1) AS regular_price_value, 
discount_price, 
url_page, 
datetime(crawled_runtime) AS crawled_runtime, 
crawled_url, 
strftime('%Y %m %d', crawled_runtime) AS crawled_date
FROM tbl_alpha;

/* 

DROP VIEW view_gamma;

SELECT 
regular_price, 
regular_price_value
FROM view_gamma 
LIMIT 5;

*/