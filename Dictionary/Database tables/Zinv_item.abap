@EndUserText.label : 'Inventory Items'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table zinv_item {

  key client  : abap.clnt not null;
  key item_id : abap.char(30) not null;
  name        : abap.char(100);
  quantity    : abap.int4;

}
