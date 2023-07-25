.read data.sql


CREATE TABLE average_prices AS
  SELECT category, AVG(MSRP) AS average_price FROM products GROUP BY category;


CREATE TABLE lowest_prices AS
  SELECT store, item, MIN(price) AS price FROM inventory GROUP BY item;


CREATE TABLE shopping_list AS
  SELECT a.item, a.store FROM
    lowest_prices AS a, products AS b  WHERE b.name = a.item GROUP BY b.category HAVING MIN(b.MSRP/b.rating);


CREATE TABLE total_bandwidth AS
  SELECT SUM(a.MBs) FROM
    stores AS a, shopping_list AS b WHERE b.store = a.store;    

