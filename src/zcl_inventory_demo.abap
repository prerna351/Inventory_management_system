CLASS zcl_inventory_demo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
ENDCLASS.




CLASS zcl_inventory_demo IMPLEMENTATION.
    METHOD if_oo_adt_classrun~main.

    DATA(lo_inventory) = NEW zcl_inventory( ).

    lo_inventory->add_item(
      i_item_id  = 'I001'
      i_name     = 'Pen'
      i_quantity = 10
    ).

    lo_inventory->add_item(
      i_item_id  = 'I002'
      i_name     = 'Book'
      i_quantity = 5
    ).
    lo_inventory->add_item(
    i_item_id  = 'I004'
    i_name     = 'Invalid'
    i_quantity = -5
    ).

    lo_inventory->add_item(
      i_item_id  = 'I003'
      i_name     = 'Marker'
      i_quantity = 3
    ).



    out->write( |Item count: { lo_inventory->get_item_count( ) }| ).
    out->write( |Total quantity: { lo_inventory->get_total_quantity( ) }| ).

    ENDMETHOD.

ENDCLASS.
