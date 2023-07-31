with df_excluded_store as (
    select 
        branch_code 
    from analysis_cds_rbs.ms_branch_cds_rbs 
    where branch_name in ('CDS Central Embassy (No Point)','CDS Central Village')
    )

select 
--        a1.member_number
--        , DATE_PART(year, date(a1.trans_date)) as years
--        , a1.partner_code
--        , a1.dept_name
--        , a1.subdept_name
--        , a1.class_name
         a1.brand_name
        , sum(a1.spend ) AS spend
        , count(DISTINCT date(a1.trans_date)|| member_number) as visit
        , count(distinct a1.ticket) as ticket
    from
        (select
            sales.trans_date
            , sales.partner_code
            , sales.member_number
            , mpcr.dept_name
            , mpcr.subdept_name
            , mpcr.class_name
            , mpcr.brand_name
            , sales.net_price_tot as spend
            , sales.receipt_no as ticket
        from analysis_cds_rbs.sales_sku_cds_rbs sales
        left join 
            (select 
                pad_sku_id
                , case when brand_name in ('LA MER','DIOR','ESTEE LAUDER','CLARINS','SKII','KIEHLS','CHANEL','SHISEIDO'
                							,'LANCOME','CLINIQUE','SULWHASOO','SISLEY','BIOTHERM','LOCCITANE'
                                            ) then brand_name
                else 'Other' end brand_name
--                , brand_name
                , dept_name
                , subdept_name
                , class_name
                , subclass_name
                , product_desc
            from analysis_cds_rbs.ms_product_cds_rbs
            where partner_code = 'CDS'
            )mpcr
            on sales.sku_id = mpcr.pad_sku_id
        where 
            upper(sales.partner_code) in ('CDS')
            AND mpcr.class_name IN ('SKINCARE')
            and (date(sales.trans_date) between date('2022-01-01') and date('2022-06-30'))
            and sales.branch_code not in (select branch_code  from df_excluded_store)
            and sales.sku_id not in (select pad_sku_id  from analysis_cds_rbs.ms_product_cds_rbs where partner_code = 'CDS' and upper(dept_name) in ('NON-MERCHANDISE ITEM','NON MERCHANDISE ITEM','SPECIALTY'))
            and sales.member_number not in (SELECT member_number FROM analysis_cds_rbs.cds_rbs_wholesaler where member_number is not null)
            and sales.member_number not in (SELECT member_number from analysis_cds_rbs.segment2022_temp where segment = 'a.Wholesaler')
            and sales.member_number is not NULL
            ) a1
      GROUP BY 1
      ORDER BY 2 DESC 
;

------------------------------------------------
---brand switching
------------------------------------------------

with df_excluded_store as (
    select 
        branch_code 
    from analysis_cds_rbs.ms_branch_cds_rbs 
    where branch_name in ('CDS Central Embassy (No Point)','CDS Central Village')
    )

    -- sale 19 
    , df_cust_19 as (
    select 
        a1.member_number
        , DATE_PART(year, date(a1.trans_date)) as years
        , a1.partner_code
        , a1.dept_name
        , a1.subdept_name
        , a1.class_name
        , a1.brand_name

        , sum(a1.spend ) AS spend
        , count(DISTINCT date(a1.trans_date)) as visit
        , count(distinct a1.ticket) as ticket

    from
        (select
            sales.trans_date
            , sales.partner_code
            , sales.member_number
            , mpcr.dept_name
            , mpcr.subdept_name
            , mpcr.class_name
            , mpcr.brand_name
            , sales.net_price_tot as spend
            , sales.receipt_no as ticket
        from analysis_cds_rbs.sales_sku_cds_rbs sales
        left join 
            (select 
                pad_sku_id
                , case when brand_name in ('LA MER','DIOR','ESTEE LAUDER','CLARINS','SKII','KIEHLS','CHANEL','SHISEIDO'
                							,'LANCOME','CLINIQUE','SULWHASOO','SISLEY','BIOTHERM','LOCCITANE'
                                            ) then brand_name
                else 'Other' end brand_name
                , dept_name
                , subdept_name
                , class_name
                , subclass_name
                , product_desc
            from analysis_cds_rbs.ms_product_cds_rbs
            where partner_code = 'CDS'
            )mpcr
            on sales.sku_id = mpcr.pad_sku_id
        where 
            upper(sales.partner_code) in ('CDS')
            and (date(sales.trans_date) between date('2022-01-01') and date('2022-06-30'))
            and sales.branch_code not in (select branch_code  from df_excluded_store)
            and sales.sku_id not in (select pad_sku_id  from analysis_cds_rbs.ms_product_cds_rbs where partner_code = 'CDS' and upper(dept_name) in ('NON-MERCHANDISE ITEM','NON MERCHANDISE ITEM','SPECIALTY'))
            and sales.member_number not in (SELECT member_number FROM analysis_cds_rbs.cds_rbs_wholesaler where member_number is not null)
            and sales.member_number not in (SELECT member_number from analysis_cds_rbs.segment2022_temp where segment = 'a.Wholesaler')
            and sales.member_number is not null
--            and sales.member_number='9-010049511'
        --    and mpcr.class_name IN ('SKINCARE')
        )a1
    GROUP BY 
        1,2,3,4,5,6,7
    order by 
        1,2,3,4,5,6,7
    )


    -- sale 22 
        , df_cust_22 as (
    select 
        a1.member_number
        , DATE_PART(year, date(a1.trans_date)) as years
        , a1.partner_code
        , a1.dept_name
        , a1.subdept_name
        , a1.class_name
        , a1.brand_name

        , sum(a1.spend ) AS spend
        , count(DISTINCT date(a1.trans_date)) as visit
        , count(distinct a1.ticket) as ticket

    from
        (select
            sales.trans_date
            , sales.partner_code
            , sales.member_number
            , mpcr.dept_name
            , mpcr.subdept_name
            , mpcr.class_name
            , mpcr.brand_name
            , sales.net_price_tot as spend
            , sales.receipt_no as ticket
        from analysis_cds_rbs.sales_sku_cds_rbs sales
        left join 
            (select 
                pad_sku_id
                , case when brand_name in ('LA MER','DIOR','ESTEE LAUDER','CLARINS','SKII','KIEHLS','CHANEL','SHISEIDO'
                							,'LANCOME','CLINIQUE','SULWHASOO','SISLEY','BIOTHERM','LOCCITANE'
                                            ) then brand_name
                else 'Other' end brand_name
                , dept_name
                , subdept_name
                , class_name
                , subclass_name
                , product_desc
            from analysis_cds_rbs.ms_product_cds_rbs
            where partner_code = 'CDS'
            )mpcr
            on sales.sku_id = mpcr.pad_sku_id
        where 
            upper(sales.partner_code) in ('CDS')
            and (date(sales.trans_date) between date('2023-01-01') and date('2023-06-30'))
            and sales.branch_code not in (select branch_code  from df_excluded_store)
            and sales.sku_id not in (select pad_sku_id  from analysis_cds_rbs.ms_product_cds_rbs where partner_code = 'CDS' and upper(dept_name) in ('NON-MERCHANDISE ITEM','NON MERCHANDISE ITEM','SPECIALTY'))
            and sales.member_number not in (SELECT member_number FROM analysis_cds_rbs.cds_rbs_wholesaler where member_number is not null)
            and sales.member_number not in (SELECT member_number from analysis_cds_rbs.segment2022_temp where segment = 'a.Wholesaler')
            and sales.member_number is not null
--            and sales.member_number='9-010049511'
            -- and mpcr.class_name IN ('SKINCARE')
        )a1
    GROUP BY 
        1,2,3,4,5,6,7
    order by 
        1,2,3,4,5,6,7
    )


--#cust_type_nel_by_brand_name
 --#cust_total
    , df_cust_total_by_brand_name as (
        select distinct member_number,partner_code,dept_name,subdept_name, class_name,brand_name
        from (
              (select distinct member_number,partner_code,dept_name,subdept_name, class_name,brand_name from df_cust_19)
        union (select distinct member_number,partner_code,dept_name,subdept_name, class_name,brand_name from df_cust_22)
        )
    )

    --#cust_type_nel
    , df_cust_type_nel_by_brand_name as (
    select distinct
           t1.member_number
           ,t1.partner_code
           ,t1.dept_name
           ,t1.subdept_name
           ,t1.class_name
           ,t1.brand_name
           ,CASE WHEN t2.member_number IS NOT NULL AND t3.member_number IS NOT NULL THEN 'existing'
            WHEN t2.member_number IS NOT NULL AND t3.member_number IS NULL THEN 'lapsed'
            WHEN t2.member_number IS NULL AND t3.member_number IS NOT NULL THEN 'new' 
            END AS nel_type_by_brand_name
    FROM df_cust_total_by_brand_name t1
    LEFT JOIN (select distinct member_number,partner_code,dept_name,subdept_name, class_name,brand_name from df_cust_19 ) t2
        ON t1.member_number = t2.member_number
        and t1.partner_code = t2.partner_code
        and t1.dept_name = t2.dept_name
        and t1.subdept_name = t2.subdept_name
        and t1.class_name = t2.class_name
        and t1.brand_name = t2.brand_name
    LEFT JOIN (select distinct member_number,partner_code,dept_name,subdept_name, class_name,brand_name from df_cust_22 ) t3
        ON t1.member_number = t3.member_number
        and t1.partner_code = t3.partner_code
        and t1.dept_name = t3.dept_name
        and t1.subdept_name = t3.subdept_name
        and t1.class_name = t3.class_name
        and t1.brand_name = t3.brand_name
    where t1.class_name IN ('SKINCARE')
    )

  --#cust_type_nel_by_class_name
  --#cust_total
    , df_cust_total_by_class_name as (
        select distinct member_number,partner_code,dept_name,subdept_name, class_name
        from ((select distinct member_number,partner_code,dept_name,subdept_name, class_name from df_cust_19 )
        union (select distinct member_number,partner_code,dept_name,subdept_name, class_name from df_cust_22 ))
    )

    --#cust_type_nel
    , df_cust_type_nel_by_class_name as (
    select distinct
           t1.member_number
           ,t1.partner_code
           ,t1.dept_name
           ,t1.subdept_name
           ,t1.class_name
           ,CASE WHEN t2.member_number IS NOT NULL AND t3.member_number IS NOT NULL THEN 'existing'
            WHEN t2.member_number IS NOT NULL AND t3.member_number IS NULL THEN 'lapsed'
            WHEN t2.member_number IS NULL AND t3.member_number IS NOT NULL THEN 'new' 
            END AS nel_type_by_class_name
            ,num_brand_name_19
            ,num_brand_name_22
    FROM df_cust_total_by_brand_name t1
    LEFT JOIN (
        select 
             member_number,partner_code,dept_name,subdept_name, class_name , count(brand_name) as num_brand_name_19
        from df_cust_19 
        group by 1,2,3,4,5
    ) t2
        ON t1.member_number = t2.member_number
        and t1.partner_code = t2.partner_code
        and t1.dept_name = t2.dept_name
        and t1.subdept_name = t2.subdept_name
        and t1.class_name = t2.class_name

    LEFT JOIN (
        select 
             member_number,partner_code,dept_name,subdept_name, class_name , count(brand_name) as num_brand_name_22
        from df_cust_22 
        group by 1,2,3,4,5
        ) t3
        ON t1.member_number = t3.member_number
        and t1.partner_code = t3.partner_code
        and t1.dept_name = t3.dept_name
        and t1.subdept_name = t3.subdept_name
        and t1.class_name = t3.class_name

    where t1.class_name IN ('SKINCARE')
    )


--#cust_type_nel
 --#cust_total
    , df_cust_total as (
        select distinct member_number,partner_code
        from ((select distinct member_number,partner_code from df_cust_19 )
        union (select distinct member_number,partner_code from df_cust_22 ))
    )

    --#cust_type_nel
    , df_cust_type_nel as (
    select distinct
           t1.member_number
           ,t1.partner_code
           ,CASE WHEN t2.member_number IS NOT NULL AND t3.member_number IS NOT NULL THEN 'existing'
            WHEN t2.member_number IS NOT NULL AND t3.member_number IS NULL THEN 'lapsed'
            WHEN t2.member_number IS NULL AND t3.member_number IS NOT NULL THEN 'new' 
            END AS nel_type
    FROM df_cust_total t1
    LEFT JOIN (select distinct member_number,partner_code from df_cust_19 ) t2
        ON t1.member_number = t2.member_number
        and t1.partner_code = t2.partner_code
    LEFT JOIN (select distinct member_number,partner_code from df_cust_22 ) t3
        ON t1.member_number = t3.member_number
        and t1.partner_code = t3.partner_code
    )


, df_output_1 as (
    select 
        t2.member_number
        ,t3.nel_type
        ,t2.partner_code
		, t1.dept_name as dept_name_19
        , t1.subdept_name as subdept_name_19
        , t5.nel_type_by_class_name
        , t1.class_name as class_name_19
        , t1.brand_name as brand_name_19
        , t4.nel_type_by_brand_name
        , t1.spend as spend_19
        , t1.visit as visit_19
        , t1.ticket as ticket_19
        
		, t2.dept_name as dept_name_22
        , t2.subdept_name as subdept_name_22
        , t2.class_name as class_name_22
        , t2.brand_name as brand_name_22
        , t2.spend as spend_22
        , t2.visit as visit_22
        , t2.ticket as ticket_22

        ,t5.num_brand_name_19
        ,t5.num_brand_name_22
    from df_cust_22 as t2
    left join df_cust_19 as t1
        on t2.member_number=t1.member_number
        and t2.partner_code=t1.partner_code
        and t2.dept_name=t1.dept_name
        and t2.subdept_name=t1.subdept_name
        and t2.class_name=t1.class_name
        and t2.class_name IN ('SKINCARE')
    INNER JOIN (select * from df_cust_type_nel ) as t3
	    on t2.member_number=t3.member_number
	    and t2.partner_code=t3.partner_code

    INNER JOIN (select * from df_cust_type_nel_by_brand_name ) as t4
    	on t2.member_number=t4.member_number
        and t2.partner_code=t4.partner_code
        and t2.dept_name=t4.dept_name
        and t2.subdept_name=t4.subdept_name
        and t2.class_name=t4.class_name
        and t2.brand_name=t4.brand_name

    INNER JOIN (select * from df_cust_type_nel_by_class_name ) as t5
    	on t2.member_number=t5.member_number
        and t2.partner_code=t5.partner_code
        and t2.dept_name=t5.dept_name
        and t2.subdept_name=t5.subdept_name
        and t2.class_name=t5.class_name
)

, df_output_1_new as (
select distinct
        member_number
        ,nel_type
        ,partner_code
		, null as dept_name_19
        , null as subdept_name_19
        , nel_type_by_class_name
        , null as class_name_19
        , null as brand_name_19
        , nel_type_by_brand_name
        , 0 as spend_19
        , 0 as visit_19
        , 0 as ticket_19
        
		, dept_name_22
        , subdept_name_22
        , class_name_22
        , brand_name_22
        , spend_22
        , visit_22
        , ticket_22

        ,0 as num_brand_name_19
        ,num_brand_name_22
from df_output_1
where nel_type_by_brand_name = 'new'
)

, df_output as (
    select 
        t1.member_number
        ,t3.nel_type
        ,t2.partner_code
		, t1.dept_name as dept_name_19
        , t1.subdept_name as subdept_name_19
        , t5.nel_type_by_class_name
        , t1.class_name as class_name_19
        , t1.brand_name as brand_name_19
        , t4.nel_type_by_brand_name
        , t1.spend as spend_19
        , t1.visit as visit_19
        , t1.ticket as ticket_19
        
		, t2.dept_name as dept_name_22
        , t2.subdept_name as subdept_name_22
        , t2.class_name as class_name_22
        , t2.brand_name as brand_name_22
        , t2.spend as spend_22
        , t2.visit as visit_22
        , t2.ticket as ticket_22

        ,t5.num_brand_name_19
        ,t5.num_brand_name_22
    from df_cust_19 as t1
    left join df_cust_22 as t2
        on t2.member_number=t1.member_number
        and t2.partner_code=t1.partner_code
        and t2.dept_name=t1.dept_name
        and t2.subdept_name=t1.subdept_name
        and t2.class_name=t1.class_name
        and t1.class_name IN ('SKINCARE')
    INNER JOIN (select * from df_cust_type_nel ) as t3
	    on t1.member_number=t3.member_number
	    and t1.partner_code=t3.partner_code

    INNER JOIN (select * from df_cust_type_nel_by_brand_name ) as t4
    	on t1.member_number=t4.member_number
        and t1.partner_code=t4.partner_code
        and t1.dept_name=t4.dept_name
        and t1.subdept_name=t4.subdept_name
        and t1.class_name=t4.class_name
        and t1.brand_name=t4.brand_name

    INNER JOIN (select * from df_cust_type_nel_by_class_name ) as t5
    	on t1.member_number=t5.member_number
        and t1.partner_code=t5.partner_code
        and t1.dept_name=t5.dept_name
        and t1.subdept_name=t5.subdept_name
        and t1.class_name=t5.class_name
)

, df_output_lapased as (
select distinct
        member_number
        ,nel_type
        ,partner_code
		, dept_name_19
        , subdept_name_19
        , nel_type_by_class_name
        , class_name_19
        , brand_name_19
        , nel_type_by_brand_name
        , spend_19
        , visit_19
        , ticket_19
        
		, dept_name_22
        , subdept_name_22
        , class_name_22
        , brand_name_22
        , spend_22
        , visit_22
        , ticket_22

        , num_brand_name_19
        , num_brand_name_22
from df_output
where nel_type_by_brand_name = 'lapsed'
)

, df_output_lapased_2 as (
select distinct
        member_number
        ,nel_type
        ,partner_code
		, dept_name_19
        , subdept_name_19
        , nel_type_by_class_name
        , class_name_19
        , brand_name_19
        , nel_type_by_brand_name
        , spend_19
        , visit_19
        , ticket_19
        
		, null as dept_name_22
        , null as subdept_name_22
        , null as class_name_22
        , null as brand_name_22
        , 0 as spend_22
        , 0 as visit_22
        , 0 as ticket_22

        , num_brand_name_19
        , 0 as num_brand_name_22
from df_output
where nel_type_by_brand_name = 'lapsed'
)



, df_output_f as (
    select 
    * 
    from df_output_1_new
    union
    select 
    * 
    from df_output_1
    where nel_type_by_class_name not in ('new')
--    union
--    select 
--    * 
--    from df_output_lapased
    union
    select 
    * 
    from df_output_lapased_2
)

--select 
--    * 
--    from df_output_lapased
--where 
--brand_name_22 in ('DIOR')
--member_number in ('9-010432537')
--and type_for_brand in ('a) new to class')


select
*,
case 
    -- new
     when nel_type_by_class_name in ('new') then 'a) new to class'
    -- existing
	 when (nel_type_by_class_name in ('existing')) and (nel_type_by_brand_name in ('new')) and (brand_name_19 is null) then 'b) existing cat new to brand'
	 when (nel_type_by_class_name in ('existing')) and (brand_name_19=brand_name_22) and (spend_19>spend_22) then 'c) existing - spend less'
	 when (nel_type_by_class_name in ('existing')) and (brand_name_19=brand_name_22) and (spend_19<=spend_22) then 'd) existing - spend more than equal'
	 when (nel_type_by_class_name in ('existing')) and (nel_type_by_brand_name in ('lapsed')) and (brand_name_22 is null) then 'e) existing cat lapsed to brand'

    -- switch: 1-1
     when (nel_type_by_class_name in ('existing')) and (brand_name_19<>brand_name_22) and (num_brand_name_19=1) 
      and (num_brand_name_22=1) and (spend_19>spend_22) then 'f) switch: 1-1 spend less' 
     when (nel_type_by_class_name in ('existing')) and (brand_name_19<>brand_name_22) and (num_brand_name_19=1) 
      and (num_brand_name_22=1) then 'f) switch: 1-1 spend more than equal' 
    -- switch: 1-M
     when (nel_type_by_class_name in ('existing')) and (brand_name_19<>brand_name_22) and (num_brand_name_19=1) and (num_brand_name_22>1) then 'f) switch: 1-M' 
     -- switch: M-M
     when (nel_type_by_class_name in ('existing')) and (brand_name_19<>brand_name_22) and (num_brand_name_19>1) and (num_brand_name_22>1) then 'f) switch: M-M' 
     -- switch: M-1
     when (nel_type_by_class_name in ('existing')) and (brand_name_19<>brand_name_22) and (num_brand_name_19>1) and (num_brand_name_22=1) then 'f) switch: M-1'
    --  lapsed
	 when nel_type_by_class_name in ('lapsed') then 'g) lapsed to class'
	 else 'Other' end as type_for_brand
into analysis_cds_rbs.kd_tmp_brand_switch_beauty_66_23_skincare
from df_output_f
;



SELECT top 5 *
FROM analysis_cds_rbs.kd_tmp_brand_switch_beauty_66_23_skincare

;

------------------------------------------------
-- Daft code--
-----Part1 Overview brand with customer behavior

WITH cal AS (
SELECT CASE WHEN type_for_brand IN ('a) new to class','g) lapsed to class') THEN '1) new/lapsed to class'
		WHEN type_for_brand IN ('e) existing cat lapsed to brand','b) existing cat new to brand') THEN '2) new/lapsed ex-cat_pre'
		WHEN type_for_brand IN ('c) existing - spend less','d) existing - spend more than equal') THEN '3) ex-cat/ex-brand spend less/more'
		WHEN type_for_brand IN ('f) switch: 1-1 spend less','f) switch: 1-1 spend more than equal') THEN '4) switch in-out'
		ELSE 'no_use' END AS customer_behav
		, sum(CASE WHEN brand_name_19 IN ('BIOTHERM') THEN spend_19 ELSE 0 END) loss	--change for loop
		, sum(CASE WHEN brand_name_22 IN ('BIOTHERM') THEN spend_22 ELSE 0 END) gain	--change for loop
		, gain - loss AS net_gain_loss
FROM analysis_cds_rbs.oat_tmp_brand_switch_beauty_19_22_SKINCARE_v2
WHERE (brand_name_19 IN ('BIOTHERM') OR brand_name_22 IN ('BIOTHERM'))	--change for loop
GROUP BY 1
HAVING customer_behav != 'no_use'
ORDER BY 1
)


, recal_2 AS (
SELECT '2) new/lapsed ex-cat' AS customer_behav
		, sum(CASE WHEN customer_behav IN ('2) new/lapsed ex-cat_pre') THEN loss ELSE 0 END) - sum(CASE WHEN customer_behav IN ('4) switch in-out') THEN loss ELSE 0 END) AS loss
		, sum(CASE WHEN customer_behav IN ('2) new/lapsed ex-cat_pre') THEN gain ELSE 0 END) - sum(CASE WHEN customer_behav IN ('4) switch in-out') THEN gain ELSE 0 END) AS gain
FROM cal
GROUP BY 1
)

, final_recal2 AS (
SELECT *
		, gain - loss AS net_gain_loss
FROM recal_2
)

SELECT *
FROM cal
WHERE customer_behav != '2) new/lapsed ex-cat_pre'
UNION 
SELECT *
FROM final_recal2
ORDER BY 1 ASC

;

--part 2: Switch in-out

WITH merge_brand_out AS (
SELECT brand_name_22 AS brand_name
		, sum(spend_19) sales_switch_out 
FROM analysis_cds_rbs.oat_tmp_brand_switch_beauty_19_22_SKINCARE_v2
WHERE 1=1
	AND type_for_brand  IN ('f) switch: 1-1 spend more than equal','f) switch: 1-1 spend less')
	AND brand_name_19 IN ('BIOTHERM')
	AND brand_name_22 NOT IN ('Other')
GROUP BY 1
)
	
, merge_brand_in AS (
SELECT brand_name_19 AS brand_name
		, sum(spend_22) sales_switch_in 
FROM analysis_cds_rbs.oat_tmp_brand_switch_beauty_19_22_SKINCARE_v2
WHERE 1=1
	AND type_for_brand  IN ('f) switch: 1-1 spend more than equal','f) switch: 1-1 spend less')
	AND brand_name_22 IN ('BIOTHERM')
	AND brand_name_19 NOT IN ('Other')
GROUP BY 1
)

, all_brand AS (
SELECT DISTINCT brand_name
FROM merge_brand_out
UNION
SELECT DISTINCT brand_name
FROM merge_brand_in
)

SELECT b.brand_name
		, o.sales_switch_out
		, i.sales_switch_in
		, COALESCE(i.sales_switch_in,0) - COALESCE(o.sales_switch_out,0) AS sales_net_gain_loss
FROM all_brand b
LEFT JOIN merge_brand_out o
	ON b.brand_name = o.brand_name
LEFT JOIN merge_brand_in i
	ON b.brand_name = i.brand_name
;

-----------------------------------------------------------
-----------------------------------------------------------
--test new data

--Part1 Overview brand with customer behavior(Need for-loop)

WITH cal AS (
SELECT CASE WHEN type_for_brand IN ('a) new to class','g) lapsed to class') THEN '1) new/lapsed to class'
		WHEN type_for_brand IN ('e) existing cat lapsed to brand','b) existing cat new to brand') THEN '2) new/lapsed ex-cat_pre'
		WHEN type_for_brand IN ('c) existing - spend less','d) existing - spend more than equal') THEN '3) ex-cat/ex-brand spend less/more'
		WHEN type_for_brand IN ('f) switch: 1-1 spend less','f) switch: 1-1 spend more than equal') THEN '4) switch in-out'
		ELSE 'no_use' END AS customer_behav
		, sum(CASE WHEN brand_name_19 IN ('DIOR') THEN spend_19 ELSE 0 END) loss	--change brand for loop
		, sum(CASE WHEN brand_name_22 IN ('DIOR') THEN spend_22 ELSE 0 END) gain	--change brand for loop
		, gain - loss AS net_gain_loss
FROM analysis_cds_rbs.kd_tmp_brand_switch_beauty_66_23_skincare		--change db
WHERE (brand_name_19 IN ('DIOR') OR brand_name_22 IN ('DIOR'))	--change brand for loop
GROUP BY 1
HAVING customer_behav != 'no_use'
ORDER BY 1
)


, recal_2 AS (
SELECT '2) new/lapsed ex-cat' AS customer_behav
		, sum(CASE WHEN customer_behav IN ('2) new/lapsed ex-cat_pre') THEN loss ELSE 0 END) - sum(CASE WHEN customer_behav IN ('4) switch in-out') THEN loss ELSE 0 END) AS loss
		, sum(CASE WHEN customer_behav IN ('2) new/lapsed ex-cat_pre') THEN gain ELSE 0 END) - sum(CASE WHEN customer_behav IN ('4) switch in-out') THEN gain ELSE 0 END) AS gain
FROM cal
GROUP BY 1
)

, final_recal2 AS (
SELECT *
		, gain - loss AS net_gain_loss
FROM recal_2
)

, df_output AS (
SELECT *
FROM cal
WHERE customer_behav != '2) new/lapsed ex-cat_pre'
UNION 
SELECT *
FROM final_recal2
ORDER BY 1 ASC
)

SELECT *
FROM df_output

;


-----------------
--part 2: Switch in-out (Need for-loop)

WITH merge_brand_out AS (
SELECT brand_name_22 AS brand_name
		, sum(spend_19) sales_switch_out 
FROM analysis_cds_rbs.kd_tmp_brand_switch_beauty_66_23_skincare		--change db
WHERE 1=1
	AND type_for_brand  IN ('f) switch: 1-1 spend more than equal','f) switch: 1-1 spend less')
	AND brand_name_19 IN ('BIOTHERM')		--change brand for loop
	AND brand_name_22 NOT IN ('Other')
GROUP BY 1
)
	
, merge_brand_in AS (
SELECT brand_name_19 AS brand_name
		, sum(spend_22) sales_switch_in 
FROM analysis_cds_rbs.kd_tmp_brand_switch_beauty_66_23_skincare		--change db
WHERE 1=1
	AND type_for_brand  IN ('f) switch: 1-1 spend more than equal','f) switch: 1-1 spend less')
	AND brand_name_22 IN ('BIOTHERM')		--change brand for loop
	AND brand_name_19 NOT IN ('Other')
GROUP BY 1
)

, all_brand AS (
SELECT DISTINCT brand_name
FROM merge_brand_out
UNION
SELECT DISTINCT brand_name

FROM merge_brand_in
)

, df_output AS (
SELECT b.brand_name AS to_brand
		, o.sales_switch_out
		, i.sales_switch_in
		, COALESCE(i.sales_switch_in,0) - COALESCE(o.sales_switch_out,0) AS sales_net_gain_loss
FROM all_brand b
LEFT JOIN merge_brand_out o
	ON b.brand_name = o.brand_name
LEFT JOIN merge_brand_in i
	ON b.brand_name = i.brand_name
)

SELECT *
FROM df_output

;
