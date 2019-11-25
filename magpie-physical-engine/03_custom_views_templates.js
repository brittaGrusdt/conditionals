// In this file you can create your own custom view templates

// A view template is a function that returns a view,
// this functions gets some config (e.g. trial_data, name, etc.) information as input
// A view is an object, that has a name, CT (the counter of how many times this view occurred in the experiment),
// trials the maximum number of times this view is repeated
// and a render function, the render function gets CT and the magpie-object as input
// and has to call magpie.findNextView() eventually to proceed to the next view (or the next trial in this view),
// if it is an trial view it also makes sense to call magpie.trial_data.push(trial_data) to save the trial information

// basic_stimulus: function (config, CT) {
//     return `<div class='magpie-view'>
//                     <h1 class='magpie-view-title'>${config.title}</h1>
//                     <p class='magpie-view-question magpie-view-qud'>${config.data[CT].QUD}</p>
//                     <div class='magpie-view-stimulus-container'>
//                         <div class='magpie-view-stimulus magpie-nodisplay'></div>
//                     </div>
//                 </div>`;
//   },

//  <label for="img1" class='magpie-view-picture magpie-response-picture'><img src=${config.data[CT].picture1}></label>

// stimulus_container_gen: function (config, CT) {
//   return `<div class='magpie-view'>
//                        <h1 class='magpie-view-title'>${config.title}</h1>
//                        <p class='magpie-view-question magpie-view-qud'>${config.data[CT].QUD}</p>
//                        <div class='magpie-view-stimulus-container'>
//                            <div class='magpie-view-stimulus magpie-nodisplay'></div>
//                        </div>
//                    </div>`;
// },

// stimulus_container_gen: function (config, CT) {
//   return `<div class='magpie-view'>
//                        <h1 class='magpie-view-title'>${config.title}</h1>
//                        <p class='magpie-view-question magpie-view-qud'>${config.data[CT].QUD}</p>
//                        <div class='magpie-view-stimulus-container'>
//
//                           <img src=${config.data[CT].picture}>
//                        </div>
//                    </div>`;

const multi_slider_generator = {
  stimulus_container_gen: function(config, CT) {
    return `<div class='magpie-view'>
                         <h1 class='magpie-view-title'>${config.title}</h1>
                     </div>`;
  },

  //old
  // answer_container_gen: function (config, CT) {
  //   console.log(CT);
  //   const option1 = config.data[CT].optionLeft;
  //   const option2 = config.data[CT].optionRight;
  //   return `<div class='magpie-view-answer-container magpie-grid-multi-slider'>
  //
  //
  //
  //           <div class='magpie-view-answer-container'>
  //             <p class='magpie-view-question' id = 'question1' >${config.data[CT].question1}</p>
  //               <span class='magpie-response-slider-option'>${option1}</span>
  //               <input type='range' id='response1' name='answer1' class='magpie-response-slider' min='0' max='100' value='50'/>
  //               <span class='magpie-response-slider-option'>${option2}</span>
  //           </div>
  //
  //
  //           <div class='magpie-view-answer-container'>
  //             <p class='magpie-view-question' id = 'question2' >${config.data[CT].question2}</p>
  //               <span class='magpie-response-slider-option'>${option1}</span>
  //               <input type='range' id='response2' name='answer2' class='magpie-response-slider' min='0' max='100' value='50'/>
  //               <span class='magpie-response-slider-option'>${option2}</span>
  //           </div>
  //
  //
  //           <div class='magpie-view-answer-container'>
  //             <p class='magpie-view-question' id = 'question3' >${config.data[CT].question3}</p>
  //               <span class='magpie-response-slider-option'>${option1}</span>
  //               <input type='range' id='response3' name='answer3' class='magpie-response-slider' min='0' max='100' value='50'/>
  //               <span class='magpie-response-slider-option'>${option2}</span>
  //           </div>
  //
  //
  //           <div class='magpie-view-answer-container'>
  //             <p class='magpie-view-question' id = 'question4' >${config.data[CT].question4}</p>
  //               <span class='magpie-response-slider-option'>${option1}</span>
  //               <input type='range' id='response4' name='answer4' class='magpie-response-slider' min='0' max='100' value='50'/>
  //               <span class='magpie-response-slider-option'>${option2}</span>
  //           </div>
  //
  //
  //         </div>
  //               <button id='next' class='magpie-view-button magpie-nodisplay'>Next</button>
  //         `;
  // },

  answer_container_gen: function(config, CT) {
    console.log(CT);
    const option1 = config.data[CT].optionLeft;
    const option2 = config.data[CT].optionRight;
    return `<div class='magpie-multi-slider-grid'>

<div class='magpie-grid-picture' >
  <img class='image-stretch' src=${config.data[CT].picture}>
</div>

 <p class='magpie-grid-qud magpie-view-question magpie-view-qud'>${config.data[CT].QUD}</p>

<div class = 'magpie-grid-slider'>
            <div class='magpie-view-answer-container'>
              <p class='magpie-view-question' id = 'question1' >${config.data[CT].question1}</p>
                <span class='magpie-response-slider-option'>${option1}</span>
                <input type='range' id='response1' name='answer1' class='magpie-response-slider' min='0' max='100' value='50'/>
                <span class='magpie-response-slider-option'>${option2}</span>
            </div>


            <div class='magpie-view-answer-container'>
              <p class='magpie-view-question' id = 'question2' >${config.data[CT].question2}</p>
                <span class='magpie-response-slider-option'>${option1}</span>
                <input type='range' id='response2' name='answer2' class='magpie-response-slider' min='0' max='100' value='50'/>
                <span class='magpie-response-slider-option'>${option2}</span>
            </div>


            <div class='magpie-view-answer-container'>
              <p class='magpie-view-question' id = 'question3' >${config.data[CT].question3}</p>
                <span class='magpie-response-slider-option'>${option1}</span>
                <input type='range' id='response3' name='answer3' class='magpie-response-slider' min='0' max='100' value='50'/>
                <span class='magpie-response-slider-option'>${option2}</span>
            </div>


            <div class='magpie-view-answer-container'>
              <p class='magpie-view-question' id = 'question4' >${config.data[CT].question4}</p>
                <span class='magpie-response-slider-option'>${option1}</span>
                <input type='range' id='response4' name='answer4' class='magpie-response-slider' min='0' max='100' value='50'/>
                <span class='magpie-response-slider-option'>${option2}</span>
            </div>
      </div>

          </div>
                <button id='next' class='magpie-view-button magpie-nodisplay'>Next</button>
          `;
  },

  // slider_rating: function (config, CT, magpie, answer_container_generator, startingTime) {
  //   let response;
  //
  //   $(".magpie-view")
  //     .append(answer_container_generator(config, CT));
  //
  //   response = $("#response");
  //   // checks if the slider has been changed
  //   response.on("change", function () {
  //     $("#next")
  //       .removeClass("magpie-nodisplay");
  //   });
  //   response.on("click", function () {
  //     $("#next")
  //       .removeClass("magpie-nodisplay");
  //   });
  //
  //   $("#next")
  //     .on("click", function () {
  //       const RT = Date.now() - startingTime; // measure RT before anything else
  //       let trial_data = {
  //         trial_name: config.name,
  //         trial_number: CT + 1,
  //         response: response.val(),
  //         RT: RT
  //       };
  //
  //       trial_data = magpieUtils.view.save_config_trial_data(config.data[CT], trial_data);
  //
  //       magpie.trial_data.push(trial_data);
  //       magpie.findNextView();
  //     });
  // },

  // $("input[name=answer1]:checked").val(),
  // $("input[name=answer2]:checked").val(),
  // $("input[name=answer3]:checked").val(),
  // $("input[name=answer4]:checked").val()

  handle_response_function: function(
    config,
    CT,
    magpie,
    answer_container_generator,
    startingTime
  ) {
    let response1;
    let response2;
    let response3;
    let response4;

    $(".magpie-view").append(answer_container_generator(config, CT));

    response1 = $("#response1");
    response2 = $("#response2");
    response3 = $("#response3");
    response3 = $("#response4");

    var response_flags = [0, 0, 0];

    const display_button_checker = function(response_number) {
      response_flags[response_number] = 1;

      if (response_flags.toString() == [1, 1, 1].toString()) {
        $("#next").removeClass("magpie-nodisplay");
      }
    };

    console.log(response_flags);
    console.log(display_button_checker);

    // check all 4 sliders
    response1.on("change", function() {
      response_flags[0] = 1;
      display_button_checker(0);
    });

    response2.on("change", function() {
      response_flags[1] = 1;
      display_button_checker(1);
    });
    response3.on("change", function() {
      response_flags[2] = 1;
      display_button_checker(2);
    });
    // response4.on("change", function() {
    //   response_flags[3] = 1;
    //   display_button_checker(3);
    // });

    console.log(response_flags);
    console.log(display_button_checker);

    $("#next").on("click", function() {
      const RT = Date.now() - startingTime; // measure RT before anything else
      let trial_data = {
        trial_name: config.name,
        trial_number: CT + 1,
        response: [
          $("#response1").val(),
          $("#response2").val(),
          $("#response3").val(),
          $("#response4").val()
        ], //[response1.val(), response2.val(), response3.val(), response4.val()],
        RT: RT
      };

      trial_data = magpieUtils.view.save_config_trial_data(
        config.data[CT],
        trial_data
      );
      magpie.trial_data.push(trial_data);
      magpie.findNextView();
    });
  }
};
