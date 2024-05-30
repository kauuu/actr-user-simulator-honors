# ACT-R tutorial unit 2 assignment task.
# This experiment opens a window, displays 3 characters
# with two being the same and one different, waits for a
# keypress, and then reports whether the key that was 
# pressed matches the different letter or not.

# Import the actr module for tutorial tasks

import actr

# Load the corresponding model

actr.load_act_r_model("ACT-R:tutorial;value-user-model.lisp")

# Create a variable to store the key that was pressed.

response = False

# This is the function which we will have ACT-R call when
# a key is pressed in the experiment window which is signaled
# by the output-key action.

# That action provides two parameters to the function called.
# The first is the name of the model that performed the keypress
# or None if it wasn't generated by a model, and the second
# is a string with the name of the key that was pressed.


def model_gives_vocal_response (model, text):

    # just store the vocal response that was spoken in the response variable

    global response
    if not response:
        response = text
    else: 
        response += '/n' + text



def multiple_experiment(n=1):

    for i in range(n):
        result = experiment(False)


# This is the function that runs the experiment for either a
# person or a model.  It has one optional parameter which if
# provided as True will run a person.
# If it is not provided or any other value is specified then
# it will run the model.

def experiment (human=False):
  
    # Reset the ACT-R system and any models that are defined to
    # their initial states.

    actr.reset()

    # Create variable for the items needed to run the exeperiment:
    #   items - a randomized list of letter strings which is randomized
    #           using the ACT-R function permute_list
    #   target - the first string from the randomized list which will be the
    #            one that is different in the display
    #   foil   - the second item from the list which will be displayed 
    #            twice
    #   window - the ACT-R window device list returned by using the ACT-R
    #            function open_exp_window to create a new window for 
    #            displaying the experiment 
    #   text# - three text items that will hold the letters to be 
    #           displayed all initialized to the foil letter to start
    #   index - a random value from 0-2 generated from the actr.random
    #           function which is used to determine which of the three
    #           text variables will be set to the target
  
    # items = actr.permute_list(["SelfDirection", "Benevolence", "Achievement", "Conformity"])
    # The two options that will be presented to the user, it is statically linked to the images used below. It does not change between runs.
    option_1 = "Conformity"
    option_2 = "SelfDirection"
    # target = items[0]
    window = actr.open_exp_window("Value Choices Experiment", width=1200, height=1125)
    
    # Model the experiment environment as a visual interface
    
    # actr.add_text_to_exp_window(window, option_1, x=50, y=100)
    # actr.add_text_to_exp_window(window, option_2, x=200, y=100)
    # actr.add_text_to_exp_window(window, "R", x=150, y=50, font_size=20)
    # actr.add_text_to_exp_window(window, "O", x=150, y=150, font_size=20)
    actr.add_image_to_exp_window(window, "robot", "robot.png", x=500, y=25,height=300,width=205,clickable=False)
    actr.add_image_to_exp_window(window, "Conformity: You eat your own lunch and accept your fate and does not try to help yourself!", "1l.png", x=0, y=374.5,height=225,width=400,clickable=False)
    actr.add_image_to_exp_window(window, "SelfDirection: Try to trade with your classmate but your mom might be a bit hurt!", "1r.png", x=800, y=374.5,height=225,width=400,clickable=False)
    actr.add_text_to_exp_window(window, "Other", x=600, y=750)

    # The introduction speach from the robot
    actr.new_word_sound("Hey! I am NAO robot. It is great to meet you!", time_in_ms=5000, onset=0)

    # The introduction speach from the robot
    actr.new_word_sound("Let us quickly start our conversation, we have a lot to discuss.", time_in_ms=5000, onset= 7000)

    actr.new_word_sound("I will describe the task now. Time for lunch! You are hungry but what your mum made for your lunch box today doesn't look very tasty. You see that one of your classmates with whom you don't talk much, has tasty pancakes in his lunch box and doesn't seem to be hungry. Would you eat your own lunch and accept your fate and does not try to help yourself? Or would you try to trade with your classmate but your mom might be a bit hurt?", time_in_ms=25000, onset = 14000)

    actr.new_word_sound("What is your choice?", time_in_ms=5000, onset = 40000)

    actr.new_word_sound("Thank you for your time, and good bye!", time_in_ms=5000, onset = 50000)

    actr.new_word_sound("Quality", time_in_ms=3000, onset = 100000)

    # Create a command in ACT-R that corresponds to our respond_to_key_press
    # function so that ACT-R is able to use the function.

    actr.add_command("model-vocal-output", model_gives_vocal_response,
                      "User Model responds with a vocal choice")

    # Monitor the output-key action so that when an output-key happens
    # our function is called.

    actr.monitor_command("output-speech","model-vocal-output")
    
    # Set the response value to '' to remove any value it may
    # have from a previous run of the experiment.

    global response
    response = ''
    
    # Here is where we actually "run" the experiment.
    # It either waits for a person to press a key or runs ACT-R
    # for up to 10 seconds giving the model a chance to do the
    # experiment.

    if human == True:

        # If a person is doing the task then for safety 
        # we make sure there is a visible window that they
        # can use to do the task, and if so, loop until the
        # response variable is not '' calling the ACT-R
        # process_events function to allow the system a 
        # chance to handle any interactions.

        if actr.visible_virtuals_available():
            while response == '':
                actr.process_events()
      
    else:

        # If it is not a human then use install_device so that
        # the features in the window will be seen by the model
        # (that will also automatically provide the model with
        # access to a virtual keyboard and mouse).  Then use
        # the ACT-R run function to run the model for up to 10
        # seconds in real-time mode.

        actr.install_device(window)

        actr.run(time=110, real_time=True)

        print_buffer_contents('imaginal')
        print_buffer_contents('goal')
        
        

    # To avoid any issues with our function for keypresses in this
    # experiment interfering with other experiments we should stop
    # monitoring output-key and then remove our command.

    actr.remove_command_monitor("output-speech","model-vocal-output")
    actr.remove_command ("model-vocal-output")

    # If the response matches the target return True otherwise
    # return False.
    
    return response

# Define the function to print buffer contents
def print_buffer_contents(buffer_name):
    """
    Print the contents of the specified buffer.
    
    :param buffer_name: The name of the buffer to print.
    """
    chunk = actr.buffer_read(buffer_name)
    if chunk:
        print(f"Buffer {buffer_name} contents: {chunk}")
    else:
        print(f"Buffer {buffer_name} is empty.")