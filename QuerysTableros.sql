/*EXT_FORECAST_DATA + COLLECTION*/

/*NEW FORECAST*/
select enterprise_id, form_id, record_id
, 'FORECAST' as RECORD_TYPE
, SURVEY_DATE as "date"
, "general_coffee_forecast_purchase_volume" as "quantity"
, "general_coffee_forecast_purchase_value" as "amount"
from DAVIDANDFAM_SCHEMA.MAP_GENERAL
where coalesce("general_coffee_forecast_purchase_volume","general_coffee_forecast_purchase_value") is not null

UNION ---------------------------------------------

select ENTERPRISE_ID, FORM_ID, RECORD_ID
, 'COLLECTION_GOOD' as RECORD_TYPE
, SURVEY_DATE as "date"
, "general_coffee_collection_good_quality_volume" as "quantity"
, "general_coffee_collection_subtotal_amount_good_rwf" as "amount"
from DAVIDANDFAM_SCHEMA.MAP_GENERAL
where coalesce("general_coffee_collection_good_quality_volume","general_coffee_collection_subtotal_amount_good_rwf") is not null

UNION ---------------------------------------------

select ENTERPRISE_ID, FORM_ID, RECORD_ID
, 'COLLECTION_OTHER' as RECORD_TYPE
, SURVEY_DATE as "date"
, "general_coffee_collection_other_quality_volume" as "quantity"
, "general_coffee_collection_subtotal_amount_other_rwf" as "amount"
from DAVIDANDFAM_SCHEMA.MAP_GENERAL
where coalesce("general_coffee_collection_other_quality_volume","general_coffee_collection_subtotal_amount_other_rwf") is not null




QUALITY_GENERAL_MAPPED_QUESTIONS


select PROFILE_ID, FORM_ID, RECORD_ID
     , ENTERPRISE_ID, PRODUCER_ID, CYCLE_YEAR, SURVEY_DATE
     , "general_coffee_collection_wet_mill_site" as general_coffee_collection_wet_mill_site
     , "general_coffee_collection_wet_mill_name" as general_coffee_collection_wet_mill_name
     , 'good' as QUALITY
     , "general_coffee_collection_good_quality_volume" as general_coffee_collection_volume
     , "general_coffee_collection_good_quality_purchased_volume" as general_coffee_collection_purchased_volume
     , "general_coffee_collection_subtotal_amount_good_rwf" as general_coffee_collection_subtotal_amount_rwf
from DAVIDANDFAM_SCHEMA.MAP_GENERAL
where coalesce("general_coffee_collection_good_quality_volume","general_coffee_collection_good_quality_purchased_volume") is not null

UNION ---------------------------------------------

select PROFILE_ID, FORM_ID, RECORD_ID
     , ENTERPRISE_ID, PRODUCER_ID, CYCLE_YEAR, SURVEY_DATE
     , "general_coffee_collection_wet_mill_site" as general_coffee_collection_wet_mill_site
     , "general_coffee_collection_wet_mill_name" as general_coffee_collection_wet_mill_name
     , 'other' as QUALITY
     , "general_coffee_collection_other_quality_volume" as general_coffee_collection_volume
     , "general_coffee_collection_other_quality_purchased_volume" as general_coffee_collection_purchased_volume
     , "general_coffee_collection_subtotal_amount_other_rwf" as general_coffee_collection_subtotal_amount_rwf
from DAVIDANDFAM_SCHEMA.MAP_GENERAL
where coalesce("general_coffee_collection_other_quality_volume","general_coffee_collection_other_quality_purchased_volume") is not null




QUALITY_PRODUCERS_MAPPED_QUESTIONS

/*NEW*/
select PROFILE_ID, FORM_ID, RECORD_ID
     , ENTERPRISE_ID, PRODUCER_ID, CYCLE_YEAR, SURVEY_DATE
     , "producer_coffee_collection_site" as producer_coffee_collection_site
     , "producer_coffee_collection_wetmill_name" as producer_coffee_collection_wetmill_name
     , 'good' as QUALITY
, "producer_coffee_collection_good_quality_volume" as producer_coffee_collection_volume
, "producer_coffee_collection_subtotal_amount_good_rwf" as producer_coffee_collection_subtotal_amount_rwf
, "producer_org_member" as producer_org_member
from DAVIDANDFAM_SCHEMA.MAP_PRODUCER
where coalesce("producer_coffee_collection_good_quality_volume","producer_coffee_collection_subtotal_amount_good_rwf") is not null

UNION ---------------------------------------------

select PROFILE_ID, FORM_ID, RECORD_ID
     , ENTERPRISE_ID, PRODUCER_ID, CYCLE_YEAR, SURVEY_DATE
     , "producer_coffee_collection_site" as producer_coffee_collection_site
     , "producer_coffee_collection_wetmill_name" as producer_coffee_collection_wetmill_name
     , 'other' as QUALITY
, "producer_coffee_collection_other_quality_volume" as producer_coffee_collection_volume
, "producer_coffee_collection_subtotal_amount_other_rwf" as producer_coffee_collection_subtotal_amount_rwf
, "producer_org_member" as producer_org_member
from DAVIDANDFAM_SCHEMA.MAP_PRODUCER
where coalesce("producer_coffee_collection_other_quality_volume","producer_coffee_collection_subtotal_amount_other_rwf") is not null

