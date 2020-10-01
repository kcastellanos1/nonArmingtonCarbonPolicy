# Non-Armington Trade Model
This is the program used in the paper "Carbon Policy in a Non-Armington Framework". It is used to simulate a non-Armington trade model of carbon pricing and trade policy.

The code is all written in Fortran. There are two data files that are also included that serve as inputs to the program: input.txt and transport.txt.
Input.txt contains all the information to calibrate the production functions and household factor supply and consumption demand functions. These include input-output matrices as well as the elasticity parameters that were set exogenously. Note that there are options for the elasticity to differ by both region and elasticity. In the input file I include these are assumed to be equal across all regions and industries. This makes the analysis simpler, but different parameters can be specified.
The taxes on carbon are stated in 2011 US dollars per ton of CO2.
Transport.txt is the transportation matrices that the model is calibrated to match. This file also includes the elasticity between distance and transportation costs that determines how much international transportation costs increase with respect to a change in distance shipped.

To run simply compile the code and place both data files in the same folder as the executable. The file should run for the policy you have chosen, and the program will create a file "output.txt" that has the equilibrium output.
In the ouput.txt file, first all variables are reprinted to ensure that they were read by the program accurately. Next, some algorithmic performance variables are printed and then economic and emissions variables are printed for each country. In the baseline scenario, all economic variables are stated in billions of 2011 US dollars and emissions are stated in gigatons of CO2.

I cannot host an executable on my website, so email me if you would like the executable file at: kcastellanos1@gsu.edu

For more information on how the model was built and the data used to calibrate it, please see my paper hosted at my website:
www.sites.gsu.edu/kcastellanos1
