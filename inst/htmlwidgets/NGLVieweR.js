HTMLWidgets.widget({

  name: 'NGLVieweR',

  type: 'output',

  factory: function(el, width, height) {

      var stage = new NGL.Stage(el);
      var structure = null;

    return {
      renderValue: function(opts) {

      //In case of Rerendering remove all components
      stage.removeAllComponents();

      //Hande resizing
      if (HTMLWidgets.shinyMode) {
      $(window).resize(function(){
      stage.handleResize();
      });
      } else {
      window.addEventListener( "resize", function( event ){
      stage.handleResize();
      }, false );
      }

      //Load timer for rendering
      //console.time("load-to-render");

      //Set stage parameters
      stage.setParameters(opts.stageParameters);
      stage.setQuality(opts.setQuality);
      stage.setFocus(opts.setFocus);

      //Load pdb from file or PDB identifier
      if(opts.type == "code"){
        structure = stage.loadFile(opts.data);
      }
      if (opts.type == "file"){
        structure = stage.loadFile(new Blob([opts.data], {type: 'text/plain'}), {
         ext:opts.file_ext});
      }

      //Load representation inputs
      var representation = opts.addRepresentation;
      var arrType = representation.type;
      var arrValues = representation.values;
      //Convert array values
      arrHandler(arrValues, "color");

      structure.then(function(o) {
        //Apply to all values objects
        arrValues.forEach((value, index) => {
          let type = arrType[index];
          o.addRepresentation(type, value)
        });

        var spheres = opts.spheres
        console.log("length of sphere buffer: " + spheres.position.length);

        if (spheres.position.length > 0) {
          // https://nglviewer.org/ngl/api/classes/bufferrepresentation.html
          var sphereBuffer = new NGL.SphereBuffer({
            position: spheres.position,
            color: spheres.color,
            radius: spheres.radius
          });
          o.addBufferRepresentation(sphereBuffer, { opacity: spheres.opacity });
        }

        o.autoView();

        //Set Scale
        o.setScale(opts.setScale)

        //Set Rotation
        if (Object.keys(opts.setRotation).length > 0) {
          o.setRotation([opts.setRotation.x,opts.setRotation.y,opts.setRotation.z])
        }

        //Set position
        if (Object.keys(opts.setPosition).length > 0) {
          o.setPosition([opts.setPosition.x,opts.setPosition.y,opts.setPosition.z])
        }

        //Set zoomMove
        var zoomMoveOpts = opts.zoomMove
        if (typeof(zoomMoveOpts.zoom) !== 'undefined') {
          var center = o.getCenter(zoomMoveOpts.center)
          var zoom = o.getZoom(zoomMoveOpts.zoom) + zoomMoveOpts.z_offSet;
          stage.animationControls.zoomMove(center, zoom, zoomMoveOpts.duration);
        }

      //Send AA sequence to Shiny
      //var aa = o.structure.getSequence()
      //Shiny.onInputChange(`${el.id}_sequence`, aa);
      if (HTMLWidgets.shinyMode) {
        var sequence = [];
        var resno = [];
        var chainname = [];
        o.structure.eachResidue( rp => {
          var code = rp.getResname1() // The single-letter code, use `rp.resname` for 3-letter
          if (code !== 'X') {
            sequence.push(code);
            resno.push(rp.resno);
            chainname.push(rp.chainname);
          }
        })

        // Write PDB
        var writer = new NGL.PdbWriter(o.structure)
        var PDBdata = writer.getData()
        var PDBblob = writer.getBlob()

        Shiny.onInputChange(`${el.id}_PDB`, PDBdata)
        Shiny.onInputChange(`${el.id}_sequence`, sequence)
        Shiny.onInputChange(`${el.id}_resno`, resno)
        Shiny.onInputChange(`${el.id}_chainname`, chainname)
      }
      });

     //<---------------------------// Shiny inputs // ---------------------------------->
     if (HTMLWidgets.shinyMode) {
      //Send rendering status to shiny
      stage.tasks.signals.countChanged.add(function(count){
        if (count > 0) {
          Shiny.onInputChange(`${el.id}_rendering`, true);
        } else
        try {
          Shiny.onInputChange(`${el.id}_rendering`, false);
        } catch (e) {
          //already removed
        }
      });


      stage.signals.clicked.add(function(pickingProxy){
        if(pickingProxy) {

      //Get AA selection details
        Shiny.onInputChange(`${el.id}_selection`, pickingProxy.getLabel());

      //Get list of surrounding atoms
      if(pickingProxy.atom){
        structure.then(function(o){

          var proximity = 3;
          var proxlevel = "residue";

          if(typeof(opts.selectionParameters) !== 'undefined'){
          var proximity = opts.selectionParameters["proximity"];
          var proxlevel = opts.selectionParameters["level"];
          }

          var atom = pickingProxy.atom;
          var atomName = atom.qualifiedName().split("]").pop()
          var resiName = atomName.split('.')[0]
          if(proxlevel == "residue"){
          var selection = new NGL.Selection(resiName);
          } else {
          var selection = new NGL.Selection(atomName);
          }

          var atomSet = o.structure.getAtomSetWithinSelection(selection, proximity);
          var atomSet2 = o.structure.getAtomSetWithinGroup( atomSet );

        Shiny.onInputChange(`${el.id}_selAround`, atomSet2.toSeleString());

        })
      }
        }
      });
     }

    //<---------------------------------// // ------------------------------------------>

     //setRock
     if(opts.setRock === true){
     stage.setRock(true);
     }
     //toggleRock
     if(opts.toggleRock === true){
     stage.toggleRock(true);
     }
     //setSpin
     if(opts.setSpin === true){
     stage.setSpin(true);
     }
     //toggleSpin
     if(opts.toggleSpin === true){
     stage.toggleSpin(true);
     }

   },

      resize: function(width, height) {
        // Code to re-render the widget with a new size
      },

      //Make stage and structure globally available
      getStage: function(){
        return stage;
      },

      getStructure: function(){
        return structure;
      }

    };
  }
});

//<---------------------------// Shiny handlers // ---------------------------------->

if (HTMLWidgets.shinyMode) {

Shiny.addCustomMessageHandler('NGLVieweR:updateRock', function(message){

  var stage = getNGLStage(message.id);

  if(typeof(stage) !== "undefined"){
    stage.setRock(message.rock);
  }

});

Shiny.addCustomMessageHandler('NGLVieweR:snapShot', function(message){
  Shiny.onInputChange(`${message.id}_rendering`, true);
  setTimeout(function() {

  var stage = getNGLStage(message.id);

  if(typeof(stage) !== "undefined"){
  stage.makeImage(message.param).then(function (blob) {
      NGL.download(blob, message.fileName);

    });
  }

Shiny.onInputChange(`${message.id}_rendering`, false);

  }, 10);
});

Shiny.addCustomMessageHandler('NGLVieweR:updateSpin', function(message){

  var stage = getNGLStage(message.id);
  if(typeof(stage) !== "undefined"){
    stage.setSpin(message.spin);
  }

});

Shiny.addCustomMessageHandler('NGLVieweR:updateFullscreen', function(message){

  var stage = getNGLStage(message.id);
  if(typeof(stage) !== "undefined"){
    stage.toggleFullscreen(document.body);
  }

});

Shiny.addCustomMessageHandler('NGLVieweR:updateZoomMove', function(message){

  var structure = getNGLStructure(message.id);
  var stage = getNGLStage(message.id);

 if(typeof(structure) !== "undefined"){

  structure.then(function(o){

      var center = o.getCenter(message.center);
      var zoom = o.getZoom(message.zoom) + message.z_offSet;

      stage.animationControls.zoomMove(center, zoom, message.duration);
 })
}

});

Shiny.addCustomMessageHandler('NGLVieweR:updateFocus', function(message){

  var stage = getNGLStage(message.id);
  if(typeof(stage) !== "undefined"){
    stage.setFocus(message.focus);
  }

});

Shiny.addCustomMessageHandler('NGLVieweR:updateSelection', function(message){

  var stage = getNGLStage(message.id);

  if(typeof(stage) !== "undefined"){

    stage.getRepresentationsByName(message.name).setSelection(message.sele);
  }

});

Shiny.addCustomMessageHandler('NGLVieweR:removeSelection', function(message){

 var stage = getNGLStage(message.id);

 if(typeof(stage) !== "undefined"){
  stage.getRepresentationsByName(message.name).dispose();
 }

});

Shiny.addCustomMessageHandler('NGLVieweR:addSelection', function(message){

  var structure = getNGLStructure(message.id);

      var param = message.param;
      var color = param['color'];

      //Convert color values
      if(typeof color === 'object' && color !== null){
      param['color'] = colorMaker(param['color']);
      }

 if(typeof(structure) !== "undefined"){

  structure.then(function(o){

  o.addRepresentation(message.type, param);

  });
 }
});

Shiny.addCustomMessageHandler('NGLVieweR:addSphere', function(message) {

  var structure = getNGLStructure(message.id);
  var buffer = message.buffer;
  
  if (typeof(structure) !== "undefined" && buffer.position.length > 0) {
    console.log("length of buffer: " + buffer.position.length);
    structure.then(function(o) {
      var sphereBuffer = new NGL.SphereBuffer({
        position: buffer.position,
        color: buffer.color,
        radius: buffer.radius
      });
      o.addBufferRepresentation(sphereBuffer, { opacity: 1 });
    });
 }
});

Shiny.addCustomMessageHandler('NGLVieweR:updateColor', function(message){

  var stage = getNGLStage(message.id);
  var color = message.color;

Shiny.onInputChange(`${message.id}_rendering`, true);
setTimeout(function() {


  if(typeof color === 'object' && color !== null){
    color = colorMaker(color);
  }
  if(typeof(stage) !== "undefined"){
  stage.getRepresentationsByName(message.name).setColor(color);
  }

Shiny.onInputChange(`${message.id}_rendering`, false);

}, 10);

});

Shiny.addCustomMessageHandler('NGLVieweR:updateVisibility', function(message){

  var stage = getNGLStage(message.id);

Shiny.onInputChange(`${message.id}_rendering`, true);
setTimeout(function() {

  if(typeof(stage) !== "undefined"){
  stage.getRepresentationsByName(message.name).setVisibility(message.value);
  }

Shiny.onInputChange(`${message.id}_rendering`, false);

}, 10);
});

Shiny.addCustomMessageHandler('NGLVieweR:updateRepresentation', function(message){

    var stage = getNGLStage(message.id);

    var param = message.param;
    var color = param['color'];

      //Convert color values
      if(typeof color === 'object' && color !== null){
      param['color'] = colorMaker(param['color']);
      }

Shiny.onInputChange(`${message.id}_rendering`, true);
setTimeout(function() {

  if(typeof(stage) !== "undefined"){
  stage.getRepresentationsByName(message.name).setParameters(message.param);
  }

Shiny.onInputChange(`${message.id}_rendering`, false);

}, 10);
});


Shiny.addCustomMessageHandler('NGLVieweR:updateStage', function(message){

  var stage = getNGLStage(message.id);

Shiny.onInputChange(`${message.id}_rendering`, true);
setTimeout(function() {

  if(typeof(stage) !== "undefined"){
  stage.setParameters(message.param);
 }

 Shiny.onInputChange(`${message.id}_rendering`, false);

}, 10);
});
}
