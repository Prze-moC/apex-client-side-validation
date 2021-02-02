function pretius_validation( obj, file_prefix ) {

  function getErrorTemplate( attributes ) {
    return attributes.attribute07 == 'Y' ? attributes.attribute06 : attributes.attribute10;
  }

  function getErrorClass( attributes ) {
    var container = $( getErrorTemplate( attributes ) ).filter(function(){
      if ( $(this).is(':contains("#ERROR_MESSAGE#")') )
        return true;
    }),
    selector = '.'+container.attr('class').split(' ').join('.');    

    return selector;
  }

  function removeHighlights( elem ){
    elem.removeClass('pretius_highlight_error').removeClass('pretius_highlight_success');
  }

  function removeIndicators( elem ){
    elem.parent().find('.pretius_val_failed,.pretius_val_success, .validationLoaderImg').remove();
  }

  function removeErrorMessage( renderOption, pluginAttributes, parent ) {
    var elem2remove = parent.find( pluginAttributes.apexErrorClass );

    if (renderOption != 2)
      elem2remove.remove();
      
  }

  function callbackFuntion(result, pluginAttributes, eventData) {
    eval("(function(triggeringElement,itemName,validationResult, errorHTML, errorClass){"+callback+"})(triggeringElement, item_id, result.validation_result, eventData.validationPlugin.errorHTML, pluginAttributes.apexErrorClass)");  
  }

  function handlePresets( presetId ) {
    if ( presetId == 3 ) 
      removeIndicators( triggeringElement );
    else if ( presetId == 4 ) 
      removeHighlights( triggeringElement );
  };

  var 
    browserEvent = obj.browserEvent.type, 
    triggeringElement = $(obj.triggeringElement), 
    isCheckbox = triggeringElement.is(':checkbox'), 
    isRadio = triggeringElement.is(':radio'), 
    isFieldSet = triggeringElement.is('fieldset'), 
    item_id = null, 
    itemValue = null, 
    itemReference = null, 
    loaderImg = null, 
    attributes = obj.action, 
    pluginAttributes = null, 
    callback = obj.action.attribute02; 
    eventData = null, 
    xhr = null,
    fadeInTime = 500,
    animateObj = {
      'specs': {
        'opacity': 1
      },
      'options': {
        'duration': fadeInTime
      }
    };




  if ( isFieldSet ) {
    item_id = triggeringElement.attr('id');
    itemValue = $.makeArray(triggeringElement.find(':input:visible:checked').map(function(){ return this.value })).join(':');
  }
  else if ( isCheckbox || isRadio ) {
    item_id = triggeringElement.parents('fieldset').first().attr('id');
    itemValue = $.makeArray(triggeringElement.parents('fieldset').first().find(':input:visible:checked').map(function(){ return this.value })).join(':');
    
  }
  else {
    item_id = triggeringElement.attr('id');
    itemValue = triggeringElement.val();

  }
  
  itemReference = $('#'+item_id);
  loaderImg = itemReference.parent().find('.validationLoaderImg');
  
  loaderImg = loaderImg.length > 0 ? loaderImg : $('<img src="'+file_prefix+'loadingBar.gif" class="validationLoaderImg" alt="loading">');


  pluginAttributes = {
    renderOption: parseInt(attributes.attribute01),
    apexErrorClass: getErrorClass( attributes ),
    showProcessing: attributes.attribute04,
    errorPlaceHolder: attributes.attribute05,
    errorTemplate: getErrorTemplate( attributes )
  };

  eventData = {
    validationPlugin: {
      item: triggeringElement,
      itemId: item_id,
      label: $('[for='+item_id+']'),
      labelText: $('[for='+item_id+']').text(),
      passed: null,
      message: null,
      errorHTML: pluginAttributes.errorTemplate
    }
  };

  handlePresets( pluginAttributes.renderOption );

  //obsluga atrybutu processing
  if (pluginAttributes.showProcessing == 1) {
    loaderImg.remove();
    itemReference.after( loaderImg.fadeIn(fadeInTime) );
  }

  //----------------------------\\
  // DA pretius_validation_init ||
  //---------------------------//

  apex.debug('Pretius validation: validation has been started for item = '+item_id+'.');
  triggeringElement.trigger('pretius_validation_init', eventData);

  //wykonaj ajax w celu sprawdzenia walidacji
  xhr = $.ajax({
    url:'wwv_flow.show',
    type:'post',
    dataType:'json',
    //dataType:'html',
    traditional: true,
    data: {
      p_request: "NATIVE="+ obj.action.ajaxIdentifier,
      p_flow_id: $v('pFlowId'),
      p_flow_step_id: $v('pFlowStepId'),
      p_instance: $v('pInstance'),
      p_arg_names: [ item_id ],
      p_arg_values: [ itemValue ],
      x01: item_id
      //x02: 'search'
    },
    
    success: function( result, textStatus, ajaxObj ){ //Type: Function( Anything data, String textStatus, jqXHR jqXHR )
      //obsluga wygasniecia sesji apex
      var label, parent, errorText, apexError = false;
      
      label = $('label[for='+item_id+']');

      if ( $.inArray( pluginAttributes.errorPlaceHolder,  ['BEFORE_ITEM', 'AFTER_ITEM']) > -1 )
        parent = $('#'+item_id).parent();
      else {
        parent = label.parent();
      }

      if (pluginAttributes.showProcessing == 1)
        loaderImg.fadeOut(500, function(){ $(this).remove() });

      try {
        test = result.validation_result.message;
        errorText = (pluginAttributes.errorTemplate).replace('#ERROR_MESSAGE#', result.validation_result.message);
      } catch(err) {
        apexError = true;
        //nie udalo sie wychwycic oczekiwanej odpowiedzi walidacji

        if ( result.addInfo !== undefined) {
          if ( result.addInfo !== " " ) {
            alert( result.addInfo+': '+result.error );
            return void(0);
          }
          else {
            errorText = (pluginAttributes.errorTemplate).replace('#ERROR_MESSAGE#', result.error);

            result =  {
              "validation_result": {
                "time": null,
                "item": item_id,
                "passed": false,
                "message": result.error,
                "revalidate": null,
                "logs": []
              }
            };
          }
        }
      }

      removeErrorMessage( pluginAttributes.renderOption, pluginAttributes, parent );


      eventData.validationPlugin.errorHTML = errorText;
      eventData.validationPlugin.passed = result.validation_result.passed;
      eventData.validationPlugin.message = result.validation_result.message;
        
      if ( result.validation_result.revalidate  ) {
        //-------------------------------\\
        // ASSOCIATED ITEMS revalidation ||
        //------------------------------//
        apex.debug('Pretius validation: the fields ('+result.validation_result.revalidate+') will be validated as their validation are dependent on item = '+item_id+'.');

        //dorobic obsluge walidacji pol checkbox i radio bo aktualnie wykona fieldset trigger
        $( result.validation_result.revalidate ).trigger(browserEvent);
      }
      //------------------\\
      //Validacja przeszła||
      //-----------------//
      if ( result.validation_result.passed == true) {

        //--------------\\
        // REVALIDATION ||
        //-------------//        
        apex.debug('Pretius validation: validation for item = '+item_id+' ended with success.');
        triggeringElement.trigger('pretius_validation_success', eventData);

        if (pluginAttributes.renderOption == 3) {        
          removeIndicators( triggeringElement );
          triggeringElement.after( $('<img class="pretius_val_success" src="'+file_prefix+'validation_success.png" alt="validation success">').fadeIn(fadeInTime) );
        }
        else if (pluginAttributes.renderOption == 4)
          triggeringElement.addClass('pretius_highlight_success');      
      }

      //------------------------\\
      //Validacja nie znaleziona||
      //-----------------------//
      else if ( result.validation_result.passed == 'not_found') {

        //--------------------------------------\\
        //DA event pretius_validation_not_found ||
        //-------------------------------------//
        apex.debug('Pretius validation: no validation found for item = '+item_id+'.');
        triggeringElement.trigger('pretius_validation_not_found', eventData);
        
        apex.debug('Pretius validation: validation for item = '+item_id+' ended.');
        triggeringElement.trigger('pretius_validation_ended', eventData);

        return void(0);
      }

      //----------------------\\
      //Validacja nie przeszła||
      //---------------------//
      else {
        

        if ( $.inArray( pluginAttributes.renderOption, [1,3, 4]) > -1 ) {
          errorText = $(errorText).css('opacity', 0).addClass('forceDisplay');

          if (pluginAttributes.errorPlaceHolder == 'BEFORE_LABEL') {
            label.before( errorText.animate(animateObj.specs, animateObj.options) );
          }
          else if (pluginAttributes.errorPlaceHolder == 'AFTER_LABEL') {
            label.after( errorText.animate(animateObj.specs, animateObj.options) );
          }
          else if (pluginAttributes.errorPlaceHolder == 'BEFORE_ITEM') {
            triggeringElement.before( errorText.animate(animateObj.specs, animateObj.options) );
          }
          else if (pluginAttributes.errorPlaceHolder == 'AFTER_ITEM') {
            triggeringElement.after( errorText.animate(animateObj.specs, animateObj.options) );
          }
          else {
            alert('Plugin settings error. Please check plugin configuration or send e-mail to apex@pretius.com for help.');
          }

        }

        //obsługa PRESETÓW
        if (pluginAttributes.renderOption == 3) {
          removeIndicators( triggeringElement );
          triggeringElement.after( $('<img class="pretius_val_failed" src="'+file_prefix+'validation_failed.png" alt="validation failed">').fadeIn(fadeInTime) );
        }
        else if (pluginAttributes.renderOption == 4) {
          triggeringElement.addClass('pretius_highlight_error');
        }

        //------------------------------------\\
        // DA event pretius_validation_failed ||
        //-----------------------------------//
        apex.debug('Pretius validation: validation for item = '+item_id+' ended with failure.');
        triggeringElement.trigger('pretius_validation_failed', eventData);

      }

      if (pluginAttributes.renderOption == 2) {
        callbackFuntion(result, pluginAttributes, eventData);
      }

      //----------------------------------\\
      // DA event pretius_validation_ended||
      //---------------------------------//
      apex.debug('Pretius validation: validation for item = '+item_id+' ended.');
      triggeringElement.trigger('pretius_validation_ended', eventData);



      
    },
    
    error: function(jqXHR, textStatus, errorThrown){ //jqXHR jqXHR, String textStatus, String errorThrown
      alert('Error occured while retrieving AJAX data: '+textStatus+"\n"+errorThrown);
    }
  });

}

