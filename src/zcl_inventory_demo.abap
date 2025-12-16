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
      i_item_id  = 'I003'
      i_name     = 'Marker'
      i_quantity = 3
    ).

    lo_inventory->update_quantity(
        i_item_id = 'I002'
        i_new_quantity = 20
    ).

    DATA(ls_found_item) = lo_inventory->search_item( 'I002' ).

    out->write(
        |id: { ls_found_item-item_id }    |
        && |name: { ls_found_item-name }    |
        && |quantity: { ls_found_item-quantity }|
        ).
    
    out->write( |Item count: { lo_inventory->get_item_count( ) }| ).
    out->write( |Total quantity: { lo_inventory->get_total_quantity( ) }| ).
    out->write( |Updated quantity for book applied.| ).
    out->write( |Total quantity now:{ lo_inventory->get_total_quantity( ) }| ).

    lo_inventory->delete_item( 'I001' ).

    out->write( |Item I001 deleted.| ).
    out->write( |Remaining item count: { lo_inventory->get_item_count( ) }| ).

    ENDMETHOD.

ENDCLASS.
