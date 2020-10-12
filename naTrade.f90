! Basic Non-Armington model of trade
! For Border Adjustment of Carbon Taxes
! Author: Kenneth Austin Castellanos
! Email: kcastellanos1@student.gsu.edu
! Last updated 10/11/20

program nonarmington


! Multi-regional model for 5 countries and 15 industries

! Flow control
integer i,r,c,ii,tt

! Global parameters
integer, parameter :: N = 5, B = 15, F = 3, Z = 13, T = 3, Transport = 10
! N = number of regions
! 1 : China (CH)
! 2 : European Union (EU)
! 3 : North America (NM)
! 4 : United States (US)
! 5 : Rest of the World (WD)

! B = number of goods/industries
! F = fuel inputs
! Z = Intermediate inputs
! T = which of Z is transportation

! Define variables

! Dynamic state variables
double precision, dimension(N,T) :: consPath, invPath, capPath1, capPath2, incPath, iRate
double precision, dimension(T) :: consPathF, invPathF, capPathF, incPathF, iRateF
double precision, dimension(N,T) :: exDemTime1, exDemTime2
double precision, dimension(N) :: consPathCalib
double precision timePref, deprec
integer dynFlag
! consPAth = path of share of consumption for income in each period
! invPath = share of income going to investment
! capPath1, capPath2 = these are the paths of capital across time
! the 1 is the original starting point, and 2 is the "updated"
! optimal path.
! incpath = path of non-capital income for each region
! iRate = the normalized steady state interest rate
! *F postfix simply indicates arrays that the subroutine 
! time3solve can interperet
! dynFlag is an integer controlling whether I add in the dynamic effects


! Economic variables
double precision, dimension(T) :: rent
double precision, dimension(N,T) :: realRent
double precision, dimension(B,N,T) :: wage, capDem, labDem, totCapDem, totLabDem
double precision, dimension(B,N) :: prodAlpha, prodGamma, vaShare
double precision :: taxShareB, taxShareS
! rent = normalized intratemporal price of capital
! realRent = real return to capital across time
! wage = real wage in each ind/region
! capDem = capital demand per Q
! labDem = labor demand per Q
! totCapDem = total capital demand in real 2010 $s
! totLabDem = total labor demand in real 2010 $s
! prodAlpha = alpha parameter in production function
! prodGamma = shift parameter in production function
! vaShare = share of value added in production

double precision, dimension(B,N) :: calibLaborSupply, calibCapitalSupply
double precision, dimension(N) :: aggLabor, eLeis, labNu, labA, labC, aggWage, labCons
double precision, dimension(B,N,T) :: laborSupply, capitalSupply, commSupply, commDemand, exSupply
! laborSupply = labor supply
! capitalSupply = capital supply
! *calib prefix indicates calibration amounts
! commSupply = commodity supplies 
! commDemand = commodity demands
! exSupply = commSupply - commDemand

double precision, dimension(B,N,T) :: finalDemand, interDemand
! finalDemand = final demands
! interDemand = intermediate input demands

double precision, dimension(B,N,T) :: consDemand
double precision, dimension(B,N) :: consAlpha
! consDemand = consumption demand
! consAlpha = utilty function parameters on consumption

double precision, dimension(B,N,T) :: labAlloc
double precision, dimension(B,N) :: labAlpha, labGamma
! labAlpha = elasticity parameter in labor allocation function
! labAlloc = final labor allocations per sector,region,time
! labGamma = shift parameter in labor allocation function

double precision, dimension(B,N,T) :: p1, p2, p3, p4
! p1 = gross price to seller
! p2 = net price to buyer
! p3 = input tax rates (inclusive of all tariffs)
! p2 = p1*(1 + p3)

double precision, dimension(B,N,T) :: relWage
! relWage = relative wage

double precision, dimension(B,B,N) :: calibIO
double precision, dimension(B,B,N,T) :: io
! ainv = total requirements matrix for each region
! io = input-output matrix for each region

! Intermediate demands
double precision, dimension(B,N) :: intSigma, intGamma, intRho
double precision, dimension(B,N,T) :: intPrice, intShare
double precision, dimension(B,Z,N) :: intAlpha
double precision, dimension(B,Z,N,T) :: intDem
! intSigma = sigma on intermediate production composite (calculated)
! intGamma = gamma on intermediate production composite (calculated)
! intShare = share of intermediate goods in production (from iomatrix)
! intRho = rho parameter on intermediate composite (imported)
! intPrice = price of the intermediate composite for each ind/reg
! intAlpha = alphas on intermediate production (calculated)
! intDemand = demand for intermediate composite

! Fuel Composite
double precision, dimension(B,N) :: fuelSigma, fuelGamma, fuelRho
double precision, dimension(B,N,T) :: fuelPrice, fuelShare
double precision, dimension(B,F,N) :: fuelAlpha
double precision, dimension(B,F,N,T) :: fuelDem
double precision cesCost

! Material composite
double precision, dimension(B,N) :: matGamma, matSigma, matRho
double precision, dimension(B,N,T) :: matPrice
double precision, dimension(B,2,N) :: matAlpha
double precision, dimension(B,2,N,T) :: matDem, prodDem

! Transportation
double precision distAlpha, distC, tradeAcc
double precision, dimension(N,N) :: distMat
double precision, dimension(B,N,T) :: tcost, itcost, dtcost, sellDist, sellDist1, itcost1
double precision, dimension(B,N,N) :: tariff, shareCalib, tflowCalib, subsidy
double precision, dimension(B,N) :: subsidyTemp
double precision, dimension(N,B,N) :: tariffTemp
double precision, dimension(B,N,N,T) :: tflow, tflow1, share, sellShare, sellShare1
! distAlpha, distC = parameters of trade cost estimation
! tcost = trade cost multiplier per unit of output
! itcost = international portion of trade cost 
! *1 indicates the updated trade cost
! dtcost = domestic portion of trade cost
! shareCalib = calibration of shares or preference matrix
! tflowCalib = calibration of trade flows
! tariffTemp, shareTemp = temporary import matrix
! tariff = tariff rates per region
! share = trade shares by region (ind,orig,dest,time)
! tflow = trade flows (ind, orig, dest)
! sellShare = shares of destinations from origin
! buyShare = shares of origins from destination

double precision, dimension(B,N,T) :: shareS, shareD
! shareS = share of supply by each region
! shareD = share of demand by each region


double precision, dimension(F,N) :: cc
double precision, dimension(F,N,T) :: emissions, dividend
! cc = carbon coefficient
! emissions = emissions by fuel source/region
! dividend = amount paid by each fuel source

double precision, dimension(N) :: permitPrice, govAdj
double precision, dimension(N,T) :: permit2, taxRateLabor, taxRateCapital, taxRevLabor, taxRevCapital, TR, RV
! taxRateLabor = tax rate on labor
! taxRateCapital = tax rate on capital
! taxRevLabor = tax revenue on labor
! taxRevCapital = tax revenue on capital
! permitPrice = price on carbon emissions
! TR = total government transfers
! RV = total government revenue
! govAdj = government revenue adjustment parameter

double precision, dimension(B,N,T) :: taxRevTariff, taxRevSubsidy
! taxRevTariff = tax revenue from tariffs

! Algorithm vars
! Vertices = B-1 commodity prices + N Non-tradable prices + 1 capital price + N governments + 1 artificial
integer, parameter :: Q = 26
double precision, dimension(Q, Q) :: myps
double precision, dimension(Q-1) :: myiv
integer, dimension(Q) :: mylabels
integer myjout, iters, maxiters, myverbose, foo, bar, itersTime
double precision numer, minpoint
double precision, dimension(Q-1) :: exDem ! Excess demands

double precision alpha, d1, d2, d3, income

taxShareB = 1
taxShareS = 0.5

! ----------------------------------------------------------------- Import data
! Open file
open(10, file = "input.txt")
open(20, file = "output.txt")
open(30, file = "transport.txt")

! Primary io matrix
read(10,*) calibIO

do ii = 1,N
    calibIO(:,:,ii) = transpose(calibIO(:,:,ii))
end do

! Calculated value added

do ii = 1,N
    do r = 1,B
        do tt = 1, T
            vaShare(r,ii) = 1-sum(calibIO(:,r,ii))
        end do 
    end do
end do


read(10,*) prodAlpha            ! Capital share in VA
read(10,*) fuelRho              ! Fuel rho
read(10,*) intRho               ! Intermediate rho
read(10,*) matRho               ! Materials rho

read(10,*) consAlpha            ! Consumption parameter
read(10,*) calibLaborSupply     ! Calibrated labor supplies
read(10,*) calibCapitalSupply   ! Calibrated capital supplies
read(10,*) labAlpha             ! Labor alpha parameter
read(10,*) labGamma             ! Labor gamma parameter

read(10,*) tariffTemp           ! Tariff rates

do ii = 1,N
    tariff(:,:,ii) = transpose(tariffTemp(:,:,ii))
end do

read(10,*) subsidyTemp

do r = 1, B
    do j = 1, N
        do ii = 1, N
            if (j /= ii) then
                subsidy(r,j,ii) = subsidyTemp(r,ii)
            end if
        end do
    end do
end do


read(10,*) govAdj               ! Government revenue adjustment parameter
read(10,*) cc                   ! Carbon coefficient
read(10,*) permitPrice          ! Price of ton of carbon

read(10,*) consPathCalib
read(10,*) timePref
read(10,*) deprec
read(10,*) eLeis

permit2 = 0
do tt = 1,T
    permit2(:,tt) = permitPrice
end do

!read(10,*) taxRateCapital
!read(10,*) taxRateLabor

TR = 0.
RV = 0.

!! Steady state dynamic variables to start
invPath = deprec
consPath = 1 - invPath
capPath1 = 1

! Assign steady state allocations
do tt = 1,T
    laborSupply(:,:,tt) = calibLaborSupply
    do ii = 1, N
        capitalSupply(:,ii,tt) = calibCapitalSupply(:,ii)*capPath1(ii,tt)
    end do
end do

do i = 1,B
    do c = 1,N
        read(30,*) shareCalib(i,c,:)
    end do
end do

do tt = 1,T
    share(:,:,:,tt) = shareCalib
end do

do i = 1,B
    do c = 1,N
        read(30,*) tflowCalib(i,c,:)
    end do
end do

do tt = 1, T
    tflow1(:,:,:,tt) = tflowCalib
end do

read(30,*) distMat

read(30,*) distAlpha        ! Trade cost parameter alpha
read(30,*) distC            ! Trade cost parameter constant


!! Transportation parameters
do tt = 1,T
    do i = 1,B
        d1 = sum(tflow1(i,:,:,tt))
        do c = 1,N
            do r = 1,N
                tflow(i,c,r,tt) = tflow1(i,c,r,tt)/d1
            end do
        end do
    end do
end do

do tt = 1, T
    do r = 1,B
        do ii = 1,N
            d2 = sum(tflow(r,:,ii,tt))
            do c = 1,N
                sellShare(r,ii,c,tt) = tflow(r,c,ii,tt)/d2
            end do
            
            ! Calculate distance metric - "trade engagement"
            sellDist(r,ii,tt) = 0.
            do c = 1,N
                sellDist(r,ii,tt) = sellDist(r,ii,tt) + sellShare(r,ii,c,tt)*distMat(ii,c)
            end do
            
            ! Trade cost is a function of total distance
            itcost(r,ii,tt) = exp(log(sellDist(r,ii,tt))*distAlpha + distC)   ! International cost
            
            if(sellDist(r,ii,tt) <= 0)then
                itcost(r,ii,tt) = 0.
            end if
            
            dtcost(r,ii,tt) = 1 - itcost(r,ii,tt)                             ! Domestic cost
            tcost(r,ii,tt) = itcost(r,ii,tt) + dtcost(r,ii,tt)                ! Total transportation cost
            
        end do
    end do
end do




! --------------------------------------------------------------------------
!                         Labor Calibration
! --------------------------------------------------------------------------



do ii = 1, N
    
    d1 = sum(calibLaborSupply(:,ii)) + sum(calibCapitalSupply(:,ii))
    
    d2 = sum(calibLaborSupply(:,ii))/d1
    
    d3 = 1
    
    ! Calibrated price index
    do r = 1, B
        d3 = d3 * (1/consAlpha(r,ii))**consAlpha(r,ii)
    end do
    
    labNu(ii) = -eLeis(ii)
    
    labA(ii) = 1/(d3*(1-d2)**(1/labNu(ii)))
    
    labC(ii) = d1

end do

! --------------------------------------------------------------------------
!                         Production Variable Calibration
! --------------------------------------------------------------------------

! Fuel demand parameters
do tt = 1,T
do r = 1,B
    do ii = 1,N
        
        fuelSigma(r,ii) = 1/(1-fuelRho(r,ii))
        d1 = 0.
        d2 = 0.
        d3 = 0.
        
        fuelShare(r,ii,tt) = sum(calibIO(1:F,r,ii))
               
        ! Calculate the alphas
        do i = 1,F
            fuelAlpha(r,i,ii) = calibIO(i,r,ii)/fuelShare(r,ii,tt)
            fuelAlpha(r,i,ii) = fuelAlpha(r,i,ii)**(1/fuelSigma(r,ii)) ! fuelAlpha(B,F,N)
            d2 = d2 + fuelAlpha(r,i,ii)**fuelSigma(r,ii)
        end do
        
        d2 = d2**(1/(1-fuelSigma(r,ii)))
        
        ! Caclulate cost variable
        cesCost = 0.
        do i = 1,F
            cesCost = cesCost + (fuelAlpha(r,i,ii)**fuelSigma(r,ii))*(1**(1-fuelSigma(r,ii)))
        end do
        
        ! Calculate the gamma parameter
        do i = 1,F
            ! This if condition finds the first non-zero entry to use
            ! as the calibration mark. Any of them will work, but
            ! it has to be non-zero
            if (calibIO(i,r,ii) > 0.) then
                d1 = calibIO(i,r,ii)/sum(calibIO(1:F,r,ii))
                d3 = (fuelAlpha(r,i,ii)**fuelSigma(r,ii))*(1/d2)**(-fuelSigma(r,ii))
                fuelGamma(r,ii) = (d1/d3)**((1-fuelSigma(r,ii))/fuelSigma(r,ii))
            end if
        end do
        
        ! Finally calculate fuel demands to ensure fidelity
        
        cesCost = (cesCost**(1/(1-fuelSigma(r,ii))))*(fuelGamma(r,ii)**((fuelSigma(r,ii)-1)/fuelSigma(r,ii)))
        
        do i = 1,F
            fuelDem(r,i,ii,tt) = ((fuelAlpha(r,i,ii)/fuelGamma(r,ii))**fuelSigma(r,ii))*(1/cesCost)**(-fuelSigma(r,ii))
            !io(i,r,ii) = fuelDem(r,i,ii) ! Instead of saving here, I calculate totals in the next nest
        end do
        
        if(sum(calibIO(1:F,r,ii)) <= 0.000001)then
            fuelDem(r,1:F,ii,tt) = 0.
        end if
        
    end do
end do
end do

! Intermediate demand parameters
! Note that there are B-F+1 demands from this function.
! The final entry 'Z' is the total demand for fuel.
! This is then multiplied by the fuel demands for
! the fuel demand nest.
do tt = 1,T
    do r = 1,B
        do ii = 1,N
            
            intSigma(r,ii) = 1/(1-intRho(r,ii))
            d1 = 0.
            d2 = 0.
            d3 = 0
            
            intShare(r,ii,tt) = sum(calibIO(:,r,ii))
            
            ! Calculate the alphas
            do i = 1, Z-1
                intAlpha(r,i,ii) = calibIO(i+F,r,ii)/intShare(r,ii,tt)
                intAlpha(r,i,ii) = intAlpha(r,i,ii)**(1/intSigma(r,ii)) 
                d2 = d2 + intAlpha(r,i,ii)**intSigma(r,ii)
            end do
            
            intAlpha(r,Z,ii) = sum(calibIO(1:F,r,ii))/intShare(r,ii,tt)
            intAlpha(r,Z,ii) = intAlpha(r,Z,ii)**(1/intSigma(r,ii)) 
            d2 = d2 + intAlpha(r,Z,ii)**intSigma(r,ii)
            
            d2 = d2**(1/(1-intSigma(r,ii)))
            
            cesCost = 0.
            do i = 1,Z
                cesCost = cesCost + (intAlpha(r,i,ii)**intSigma(r,ii))*(1**(1-intSigma(r,ii)))
            end do
            
            cesCost = cesCost + (intAlpha(r,i,ii)**intSigma(r,ii))*(1**(1-intSigma(r,ii)))
            
            d1 = calibIO(1+F,r,ii)/sum(calibIO(:,r,ii))
            d3 = (intAlpha(r,1,ii)**intSigma(r,ii))*(1/d2)**(-intSigma(r,ii)) ! Use first good as target
            intGamma(r,ii) = (d1/d3)**((1-fuelSigma(r,ii))/fuelSigma(r,ii))
            
            cesCost = (cesCost**(1/(1-intSigma(r,ii))))*(intGamma(r,ii)**((intSigma(r,ii)-1)/intSigma(r,ii)))
            
            do i = 1,Z-1
                intDem(r,i,ii,tt) = ((intAlpha(r,i,ii)/intGamma(r,ii))**intSigma(r,ii))*(1/cesCost)**(-intSigma(r,ii))
                io(i+F,r,ii,tt) = intDem(r,i,ii,tt)
            end do
            
            intDem(r,Z,ii,tt) = ((intAlpha(r,Z,ii)/intGamma(r,ii))**intSigma(r,ii))*(1/cesCost)**(-intSigma(r,ii))
            io(1:F,r,ii,tt) = intDem(r,Z,ii,tt)*fuelDem(r,1:F,ii,tt)
            
        end do
    end do
end do

! Material composite demand parameters
! mat_1 = capital, mat_2 = intermediate
do tt = 1,T
    do r = 1,B
        do ii = 1,N
        
            matSigma(r,ii) = 1/(1-matRho(r,ii))
        
            ! Use Prod Alpha part of composite to calibrate
            d1 = sum(calibIO(:,r,ii))
            d1 = prodAlpha(r,ii)*(1-d1)/(d1 + prodAlpha(r,ii)*(1-d1)) 
            
            matAlpha(r,1,ii) = d1
            matAlpha(r,2,ii) = 1-matAlpha(r,1,ii)
            
            matAlpha(r,1,ii) = matAlpha(r,1,ii)**(1/matSigma(r,ii))
            matAlpha(r,2,ii) = matAlpha(r,2,ii)**(1/matSigma(r,ii))
            
            ! Caclulate cost variable
            cesCost = (matAlpha(r,1,ii)**matSigma(r,ii))*(1**(1-matSigma(r,ii))) + &
                   (matAlpha(r,2,ii)**matSigma(r,ii))*(1**(1-matSigma(r,ii)))
                    
            d2 = (matAlpha(r,1,ii)**matSigma(r,ii) + matAlpha(r,2,ii)**matSigma(r,ii))** &
                (1/matSigma(r,ii))
            
            d3 = (matAlpha(r,1,ii)**matSigma(r,ii))*(1/d2)**(-matSigma(r,ii))
            matGamma(r,ii) = (d1/d3)**((1-matSigma(r,ii))/matSigma(r,ii))
            
            cesCost = (cesCost**(1/(1-matSigma(r,ii))))*(matGamma(r,ii)**((matSigma(r,ii)-1)/matSigma(r,ii)))
            
            matDem(r,1,ii,tt) = ((matAlpha(r,1,ii)/matGamma(r,ii))**matSigma(r,ii))*(1/cesCost)**(-matSigma(r,ii))
            matDem(r,2,ii,tt) = ((matAlpha(r,2,ii)/matGamma(r,ii))**matSigma(r,ii))*(1/cesCost)**(-matSigma(r,ii))
            
        end do
    end do
end do

! Production composite parameters
! prod_1 = labor, prod_2 = materials

do r = 1,B
    do ii = 1,N
        d1 = sum(calibIO(:,r,ii)) 
        d1 = d1 + prodAlpha(r,ii)*(1-d1) ! Size of materials composite
             
        prodAlpha(r,ii) = 1-d1 ! The rest is labor
            
        d1 = (prodAlpha(r,ii)/(1-prodAlpha(r,ii)))**(1-prodAlpha(r,ii))
        prodGamma(r,ii) = (prodAlpha(r,ii)/d1)
    
    end do
end do

do tt = 1,T
    do r = 1,B
        do ii = 1,N
            
            d1 = (1/1) * (prodAlpha(r,ii)/(1-prodAlpha(r,ii)))
            prodDem(r,1,ii,tt) = prodGamma(r,ii)*d1**(1-prodAlpha(r,ii)) ! Labor in production
            d1 = (1/1) * ((1-prodAlpha(r,ii))/prodAlpha(r,ii))
            prodDem(r,2,ii,tt) = prodGamma(r,ii)*d1**prodAlpha(r,ii) ! Materials in production
            
            labDem(r,ii,tt) = prodDem(r,1,ii,tt)
            capDem(r,ii,tt) = prodDem(r,2,ii,tt)*matDem(r,1,ii,tt)
            io(:,r,ii,tt) = prodDem(r,2,ii,tt)*matDem(r,2,ii,tt)*io(:,r,ii,tt)
            
        end do
    end do
end do



! --------------------------------------------------------------------------
!                         Output Creation
! --------------------------------------------------------------------------

! ------------------------------------------------------------ Write out parameters to ensure fidelity

write(20,*) "-------------------------------------------------------------------------------------------------"
write(20,*) "                                      Input Variables Fidelity"
write(20,*) "-------------------------------------------------------------------------------------------------"

do r = 1,N
    write(20,*) "IO Matrix for region ",r
    write(20,*) "-----------------------------------"
    do c = 1,B
        write(20,*) calibIO(c,:,r)
    end do

    write(20,*) "Fuel Dem."
    do c = 1,B
        write(20,*) fuelDem(c,:,r,T)
    end do
    write(20,*) "Materials Dem."
    do c = 1,B
        write(20,*) matDem(c,:,r,T)
    end do
    write(20,*) "Production Dem."
    do c = 1,B
        write(20,*) prodDem(c,:,r,T)
    end do
    write(20,*) "fuelRho   =", fuelRho(:,r)
    write(20,*) "consAlpha =", consAlpha(:,r)
    write(20,*) "prodAlpha =", prodAlpha(:,r)
    write(20,*) "vaShare   =", vaShare(:,r)
    write(20,*) "Lab Alpha =", labAlpha(:,r)
    write(20,*) "Lab Gamma =", labGamma(:,r)
    write(20,*) "Carbon Co =", cc(:,r)
    write(20,*) "Gov. Adj. =", govAdj(r), "Leisure Elasticity =", eLeis(r)
    write(20,*) "Carbon Pr =", permitPrice(r)
    write(20,*) "Tr. Cost  =", tcost(:,r,T)
    
    write(20,*) "Tariff: "
    write(20,*) "-----------------------------"
    do c = 1,B
        write(20,*) tariff(c,:,r)
    end do

    write(20,*) "Trade Shares: "
    write(20,*) "-----------------------------"
    do c = 1,B
        write(20,*) share(c,r,:,1)
    end do
end do

bar = 1
itersTime = 1

dynFlag = 1

do while (bar == 1)

    do tt = 1, dynFlag
    
    myps = 0.                   ! Initialize primitive zimplex

    foo = 1                     ! Flag for exiting loop
    iters = 1                   ! Iteration counter
    maxiters = 500000           ! Max iteration to go through
    !maxiters = 1               
    myjout = 1                  ! Starting replacement column
    myverbose = 0               ! Output to the diagnostic file?   

    minpoint = 10000

    do while (foo == 1)

        if (iters == 1) then
            write(*,*) "******************** Solving economic equilibria, time period", tt, "************"
            
            myiv = 30                   ! Initial vertex starting poiint
    
            i = N + 1
            myiv(2:i) = 1
            
            do ii = 1,N
                if(permit2(ii,tt) > 0) then
                    myiv(ii+1) = 10
                    myiv(ii+6) = 30
                end if
            end do
            
            call merrill( myjout, myps, mylabels, myiv, exDem, 1, Q-1, myverbose, 2) ! Setup
        else
            call merrill( myjout, myps, mylabels, myiv, exDem, 0, Q-1, myverbose, 2) ! Continuing
        end if

        ! --------------------------------------------------------------------
        !                           Set Prices
        ! --------------------------------------------------------------------
        
        ii = 2
        !numer = sum(myps(11:25,myjout)) / (25-11)! Rent in period one is numeraire
        numer = myps(2,myjout)
            
        rent(tt) = myps(ii,myjout)/numer
        ii = ii + 1
        
        do i = 1, N
           TR(i,tt) = (myps(ii,myjout)/numer)*govAdj(i)
           ii = ii + 1
        end do
        
        ! Utilities are non-tradable
        do i = 1, N
            p1(10,i,tt) = myps(ii,myjout)/numer
            ii = ii + 1
        end do

        do i = 1,9
            p1(i,:,tt) = myps(ii,myjout)/numer
            ii = ii + 1
        end do

        do i = 11,B
            p1(i,:,tt) = myps(ii,myjout)/numer
            ii = ii + 1
        end do

        ! --------------------------------------------------------------------
        !                       Calculate net input/output prices
        ! --------------------------------------------------------------------
        ! p1 = "Net" Price or world price before tariffs
        ! p2 = "Gross" Price or price to buyer plus tariffs

        p3 = 0.
        p4 = 0.
        
        do ii = 1, N
            !p4(r,ii,tt) = 0.
            do i = 1, N
                do r = 1, B
                    p4(r,ii,tt) = p4(r,ii,tt) + subsidy(r,i,ii)*share(r,i,ii,tt)
                end do
            end do
        end do
        
        do ii = 1,N
           
            do r = 1,B
                
                ! p3 is the upcharge from tariffs
                !p3(r,ii,tt) = 0.
                do i = 1, N
                    p3(r,ii,tt) = p3(r,ii,tt) + tariff(r,i,ii)*share(r,ii,i,tt)
                end do
                
                p2(r,ii,tt) = p1(r,ii,tt)*(1 + p3(r,ii,tt))
                
                
            end do
            
            p2(1,ii,tt) = p2(1,ii,tt) + permit2(ii,tt)*cc(1,ii)
            !p2(2,ii) = p2(2,ii) + permitPrice(ii)*cc(2,ii)
            p2(7,ii,tt) = p2(7,ii,tt) + permit2(ii,tt)*cc(2,ii) ! Refined petroleum goods
            p2(3,ii,tt) = p2(3,ii,tt) + permit2(ii,tt)*cc(3,ii)
            
        end do
        
        
        ! --------------------------------------------------------------------
        !                       Calculate fuel inputs
        ! --------------------------------------------------------------------
        
        do r = 1,B
            do ii = 1,N
            
                cesCost = 0.
                do i = 1,F
                    cesCost = cesCost + (fuelAlpha(r,i,ii)**fuelSigma(r,ii))*(p2(i,ii,tt)**(1-fuelSigma(r,ii)))
                end do
                
                cesCost = (cesCost**(1/(1-fuelSigma(r,ii))))*(fuelGamma(r,ii)**((fuelSigma(r,ii)-1)/fuelSigma(r,ii)))
                
                fuelPrice(r,ii,tt) = 0.
                do i = 1,F
                    fuelDem(r,i,ii,tt) = ((fuelAlpha(r,i,ii)/fuelGamma(r,ii))**fuelSigma(r,ii))* &
                    (p2(i,ii,tt)/cesCost)**(-fuelSigma(r,ii))
                    
                    io(i,r,ii,tt) = fuelDem(r,i,ii,tt)
                    fuelPrice(r,ii,tt) = fuelPrice(r,ii,tt) + fuelDem(r,i,ii,tt)*p2(i,ii,tt)
                end do
                
            end do
        end do


        ! --------------------------------------------------------------------
        !                       Calculate intermediate inputs
        ! --------------------------------------------------------------------
        
        do r = 1,B
            do ii = 1,N
            
                cesCost = 0.
                do i = 1,Z-1
                
                    if ( i == Transport ) then ! Add in transportation cost nest
                        d1 = p2(i+F,ii,tt)
                        cesCost = cesCost + (intAlpha(r,i,ii)**intSigma(r,ii))* &
                        ((p2(i,ii,tt) * tcost(r,ii,tt) ) **(1-intSigma(r,ii)))
                    else
                        cesCost = cesCost + (intAlpha(r,i,ii)**intSigma(r,ii))* &
                        (p2(i+F,ii,tt)**(1-intSigma(r,ii)))
                    end if
                end do
                
                cesCost = cesCost + (intAlpha(r,Z,ii)**intSigma(r,ii))*(fuelPrice(r,ii,tt)**(1-intSigma(r,ii))) ! fuel composite
                
                cesCost = (cesCost**(1/(1-intSigma(r,ii))))*(intGamma(r,ii)**((intSigma(r,ii)-1)/intSigma(r,ii)))
                
                intPrice(r,ii,tt) = 0. ! Clear out intermediate price vector
                
                do i = 1,Z-1
                    intDem(r,i,ii,tt) = ((intAlpha(r,i,ii)/intGamma(r,ii))**intSigma(r,ii))* &
                    (p2(i+F,ii,tt)/cesCost)**(-intSigma(r,ii))
                    
                    if (i == Transport ) then ! Add in transportation demand
                        io(i+F,r,ii,tt) = intDem(r,i,ii,tt) * tcost(r,ii,tt)
                        intPrice(r,ii,tt) = intPrice(r,ii,tt) + intDem(r,i,ii,tt) * p2(i+F,ii,tt) * tcost(r,ii,tt)
                    else
                        io(i+F,r,ii,tt) = intDem(r,i,ii,tt)
                        intPrice(r,ii,tt) = intPrice(r,ii,tt) + intDem(r,i,ii,tt) * p2(i+F,ii,tt)
                    end if
                end do
                
                intDem(r,Z,ii,tt) = ((intAlpha(r,Z,ii)/intGamma(r,ii))**intSigma(r,ii))* & 
                (fuelPrice(r,ii,tt)/cesCost)**(-intSigma(r,ii))
                
                intPrice(r,ii,tt) = intPrice(r,ii,tt) + fuelPrice(r,ii,tt)*intDem(r,Z,ii,tt)
                    
                io(1:F,r,ii,tt) = intDem(r,Z,ii,tt)*fuelDem(r,1:F,ii,tt)
                
            end do
        end do

        
        ! --------------------------------------------------------------------
        !                       Materials Composite
        ! --------------------------------------------------------------------

        do r = 1,B
            do ii = 1,N
            
                cesCost = (matAlpha(r,1,ii)**matSigma(r,ii))*(rent(tt)**(1-matSigma(r,ii))) + &
                            (matAlpha(r,2,ii)**matSigma(r,ii))*(intPrice(r,ii,tt)**(1-matSigma(r,ii)))
                
                cesCost = (cesCost**(1/(1-matSigma(r,ii))))*(matGamma(r,ii)**((matSigma(r,ii)-1)/matSigma(r,ii)))
              
                matDem(r,1,ii,tt) = ((matAlpha(r,1,ii)/matGamma(r,ii))**matSigma(r,ii))* &
                    (rent(tt)/cesCost)**(-matSigma(r,ii))
                matDem(r,2,ii,tt) = ((matAlpha(r,2,ii)/matGamma(r,ii))**matSigma(r,ii))* &
                    (intPrice(r,ii,tt)/cesCost)**(-matSigma(r,ii))
                    
                matPrice(r,ii,tt) = matDem(r,1,ii,tt)*rent(tt) + matDem(r,2,ii,tt)*intPrice(r,ii,tt)
            
            end do
        end do


        ! --------------------------------------------------------------------
        !                       Production Composite / Solve for wages
        ! --------------------------------------------------------------------
        
        do ii = 1,N
            do r = 1,B
                
                alpha = 1-prodAlpha(r,ii)       
                           
                prodDem(r,2,ii,tt) = (alpha/matPrice(r,ii,tt))*p1(r,ii,tt)*(1 + p4(r,ii,tt)) ! Rebate for the carbon tax
                
                labDem(r,ii,tt) = 1/(( (1/prodGamma(r,ii)) * prodDem(r,2,ii,tt)**alpha)**(1/(1-alpha)))
                wage(r,ii,tt) = (prodAlpha(r,ii)/labDem(r,ii,tt))*p1(r,ii,tt)*(1 + p4(r,ii,tt)) ! Rebate for the carbon tax
                           
                if (wage(r,ii,tt) <= 0) then
                    capDem(r,ii,tt) = 0.
                    labDem(r,ii,tt) = 0.
                    wage(r,ii,tt) = 0.
                end if
                
                capDem(r,ii,tt) = prodDem(r,2,ii,tt)*matDem(r,1,ii,tt)
                io(:,r,ii,tt) = prodDem(r,2,ii,tt)*matDem(r,2,ii,tt)*io(:,r,ii,tt)
              
            end do

            !write(*,*) "va1 = ", va1(:,ii)
            !write(*,*) "wages = ", wage(:,ii)
        end do


        ! --------------------------------------------------------------------
        !                       Allocate Labor Supply
        ! --------------------------------------------------------------------
        
        do ii = 1,N
        
            d1 = sum(wage(:,ii,tt))/B
            do r = 1,B
                relWage(r,ii,tt) = wage(r,ii,tt)/d1
            end do
            
            d1 = 0.
            do r = 1,B
                labAlloc(r,ii,tt) = EXP(log(relWage(r,ii,tt))*labAlpha(r,ii) + labGamma(r,ii))
                            
                if (wage(r,ii,tt) <= 0) then
                    labAlloc(r,ii,tt) = 0.
                end if
                
                d1 = d1 + labAlloc(r,ii,tt)
                
            end do
            
            aggWage(ii) = 0.
            d2 = 1.
            d3 = 0.
            do r = 1,B
                aggWage(ii) = aggWage(ii) + wage(r,ii,tt)*(labAlloc(r,ii,tt)/d1)
                d2 = d2 * (p2(r,ii,tt)/consAlpha(r,ii))**consAlpha(r,ii)
            end do
            
            !d2 = sum(consAlpha(:,ii)) / d2
            
            aggWage(ii) = aggWage(ii)/d2
            aggLabor(ii) = (1 - (aggWage(ii)/(labA(ii)))**labNu(ii))*labC(ii)
            
            ! Redistribute allocations
            do r = 1,B
                labAlloc(r,ii,tt) = (labAlloc(r,ii,tt)/d1)*aggLabor(ii)
                !labAlloc(r,ii) = laborSupply(r,ii)
            end do
        
        end do



        ! --------------------------------------------------------------------
        !                           Consumers
        ! --------------------------------------------------------------------
        
        do ii = 1,N
            income = 0.
            do r = 1,B
             
                income = income + wage(r,ii,tt)*(1-taxRateLabor(ii,tt))*labAlloc(r,ii,tt) &
                + rent(tt)*(1-taxRateCapital(ii,tt))*capitalSupply(r,ii,tt)
                
            end do
            
            income = income + TR(ii,tt)
            
            incPath(ii,tt) = income
            
            do r = 1,B
                consDemand(r,ii,tt) = (consAlpha(r,ii) / p2(r,ii,tt)) * income
            end do
            
        end do


        ! --------------------------------------------------------------------
        !                           Final Accounting
        ! --------------------------------------------------------------------

        ! -------------------------------------------------------------- Commodity Supply/Demand

        do ii = 1,N
            do r = 1,B
                commSupply(r,ii,tt) = labAlloc(r,ii,tt)/labDem(r,ii,tt)
                
                if (labDem(r,ii,tt) <= 0 .or. wage(r,ii,tt) <= 0) then
                    commSupply(r,ii,tt) = 0
                end if
                
                finalDemand(r,ii,tt) = consDemand(r,ii,tt)
            end do
            
            interDemand(:,ii,tt) = matmul(commSupply(:,ii,tt),transpose(io(:,:,ii,tt)))
            
            commDemand(:,ii,tt) = finalDemand(:,ii,tt) + interDemand(:,ii,tt)
            exSupply(:,ii,tt) = commSupply(:,ii,tt) - commDemand(:,ii,tt)
            
        end do


        ! ------------------------------------------------------------- Factor Demands

        do ii = 1,N
            do r = 1,B
            
                totLabDem(r,ii,tt) = commSupply(r,ii,tt)*labDem(r,ii,tt)
                totCapDem(r,ii,tt) = commSupply(r,ii,tt)*capDem(r,ii,tt)
                
            end do
        end do

        
        ! --------------------------------------------------------------- Environmental Accounting

        do ii = 1,N
        
            emissions(1,ii,tt) = cc(1,ii)*commDemand(1,ii,tt)  !! Coal
            emissions(2,ii,tt) = cc(2,ii)*commDemand(7,ii,tt)  !! Ref Petro
            emissions(3,ii,tt) = cc(3,ii)*commDemand(3,ii,tt)  !! Natural Gas
            
        end do


        ! --------------------------------------------------------------- Government accounting
        
        taxRevLabor(:,tt) = 0.
        taxRevCapital(:,tt) = 0.
        taxRevTariff(:,:,tt) = 0.
        taxRevSubsidy(:,:,tt) = 0.
        dividend(:,:,tt) = 0.
        
        do ii = 1,N
            
            !taxRevLabor(ii,tt) = 0.
            !taxRevCapital(ii,tt) = 0.
            !taxRevTariff(:,ii,tt) = 0.
            !dividend(:,ii,tt) = 0.
            
            do r = 1,B
                taxRevLabor(ii,tt) = taxRevLabor(ii,tt) + taxRateLabor(ii,tt) * wage(r,ii,tt) * labAlloc(r,ii,tt)
                taxRevCapital(ii,tt) = taxRevCapital(ii,tt) + taxRateCapital(ii,tt) * rent(tt) * capitalSupply(r,ii,tt)
                taxRevTariff(r,ii,tt) = p1(r,ii,tt) * p3(r,ii,tt) * commDemand(r,ii,tt)
                taxRevSubsidy(r,ii,tt) = p1(r,ii,tt) * p4(r,ii,tt) * commSupply(r,ii,tt)
                
               ! do i = 1, N
               !     taxRevSubsidy(r,ii,tt) = p1(r,i,tt)*subsidy(r,i,ii)*share(r,i,ii,tt)*commDemand(r,i,tt) & 
               !         + taxRevSubsidy(r,ii,tt)
               ! end do
            end do
            
            do r = 1,3
                dividend(r,ii,tt) = permit2(ii,tt)*emissions(r,ii,tt)
            end do
            
            RV(ii,tt) = taxRevCapital(ii,tt) + taxRevLabor(ii,tt) + sum(taxRevTariff(:,ii,tt)) + & 
                sum(dividend(:,ii,tt)) - sum(taxRevSubsidy(:,ii,tt))
            
        end do


        ! ----------------------------------------------------------  Excess Demands
        ii = 1

        exDem(ii) = sum(totCapDem(:,:,tt)) - sum(capitalSupply(:,:,tt))
        ii = ii + 1

        do i = 1,N
            exDem(ii) = RV(i,tt) - TR(i,tt)
            ii = ii + 1
        end do

        do i = 1,N
            exDem(ii) = commDemand(10,i,tt) - commSupply(10,i,tt)
            ii = ii + 1
        end do

        do i = 1,9
            exDem(ii) = sum(commDemand(i,:,tt)) - sum(commSupply(i,:,tt))
            ii = ii + 1
        end do

        do i = 11,B
            exDem(ii) = sum(commDemand(i,:,tt)) - sum(commSupply(i,:,tt))
            ii = ii + 1
        end do


        ! ----------------------------------------------------------  Algorithm 

        if (mod(iters,50000) == 0) then
            write(*,*) "Iters = ", iters, "Goal:", sum(exDem**2)
        end if
        
        if(sum(exDem**2) < minpoint) then
            minpoint = sum(exDem**2)
        end if
        
        ! --------------------------------------------------------------------
        !                           Check excess demands
        ! --------------------------------------------------------------------
        
        if(sum(exDem**2) < 0.001) then
        
            ! If excess demands are small enough for use to be at an economic
            ! equilibrium, then I can check the transportation costs
            ! First caclulate the average distance traveled for a shipped good
            
            shareS = 0.
            shareD = 0.
            
            do r = 1,B  ! Ind.
            
                d1 = 0.
                d2 = 0.
                
                do ii = 1,N ! Reg.
                
                    d1 = d1 + commSupply(r,ii,tt)
                    d2 = d2 + commDemand(r,ii,tt)
                    
                end do
                
                do ii = 1,N
                    shareS(r,ii,tt) = commSupply(r,ii,tt)/d1 ! Share supplied by each region
                    shareD(r,ii,tt) = commDemand(r,ii,tt)/d2 ! Share bought by each region
                end do
            
            end do
        
            
            do r = 1,B
                ! Now I have the shares purchased and supplied by each region, so I can 
                ! use linear programming to find a solution that will solve the following
                ! transportation problem.
                call tpout(shareD(r,:,tt), shareS(r,:,tt), shareCalib(r,:,:), tflow1(r,:,:,tt), N)
                do ii = 1,N
                    share(r,:,ii,tt) = tflow1(r,ii,:,tt)/sum(tflow1(r,ii,:,tt))
                end do
            end do
            
            tradeAcc = 0.
            do r = 1,B
                do ii = 1,N
                    d2 = sum(tflow1(r,:,ii,tt))
                    do c = 1,N
                        sellShare1(r,ii,c,tt) = tflow1(r,c,ii,tt)/d2
                    end do
                    
                    ! Calculate distance metric - "trade engagement"
                    sellDist1(r,ii,tt) = 0.
                    do c = 1,N
                        sellDist1(r,ii,tt) = sellDist1(r,ii,tt) + sellShare1(r,ii,c,tt)*distMat(ii,c)
                    end do
                    
                    ! Trade cost is a function of total distance
                    itcost1(r,ii,tt) = exp(log(sellDist1(r,ii,tt))*distAlpha + distC)    ! International cost
                    
                    if(sellDist1(r,ii,tt) <= 0)then
                        itcost1(r,ii,tt) = 0.
                    else
                        tradeAcc = tradeAcc + abs(itcost1(r,ii,tt) - itcost(r,ii,tt))/itcost1(r,ii,tt)
                    end if
                    
                end do
            end do
            
            if(tradeAcc > 0.3)then
                iters = 0.
                itcost = itcost1
                tcost(r,ii,tt) = itcost(r,ii,tt) + dtcost(r,ii,tt)
                tflow = tflow1
                write(*,*) "Trade Acc:", tradeAcc
            else
                foo = 3
            end if
            
        end if
        
        iters = iters + 1
        
        if( iters > maxiters ) then
            foo = 2
        end if

    end do

    do ii = 1,N
        d1 = sum(p2(:,ii,tt))/B
        realRent(ii,tt) = rent(tt)/d1
    end do

    end do

    ! Dynamic solution state

    do ii = 1,N
        
        d1 = timePref ! Time preference
        d2 = deprec ! Depreciation
        
        iRate(ii,:) = (1/d1 - 1 + d2) * realRent(ii,:)/realRent(ii,1)
        iRateF = iRate(ii,:)
        
        d3 = (consPathCalib(ii)*iRateF(1) - iRateF(1) + d2) / (1/consPathCalib(ii) - 1)
        
        incPathF = incPath(ii,:)/incPath(ii,1)*d3
        call time3solve( iRateF, incPathF, d1, d2, consPathF, invPathF, capPathF,3)
        consPath(ii,:) = consPathF/(consPathF + invPathF)
        invPath(ii,:) = invPathF/(consPathF + invPathF)
        capPath2(ii,:) = capPathF/capPathF(1)
        incPath(ii,:) = incPathF
        
    end do

    ! Assign new capital allocations
    do tt = 1,T
        do ii = 1, N
            d1 = capPath2(ii,tt)
            exDemTime2(ii,tt) = capPath2(ii,tt) - capPath1(ii,tt)
            if (itersTime > 10) then
            !    d1 = ( capPath1(ii,tt)*exDemTime2(ii,tt) - capPath2(ii,tt)*exDemTime1(ii,tt) ) / & 
            !        ( exDemTime2(ii,tt) - exDemTime1(ii,tt) )
            bar = 2
            else
            
            capPath1(ii,tt) = 0.5*d1 + 0.5*capPath1(ii,tt)
            exDemTime1 = exDemTime2
            
            capitalSupply(:,ii,tt) = calibCapitalSupply(:,ii)*capPath1(ii,tt)
            end if
        end do
    end do

    write(*,*) "Time Objective:" , sum(exDemTime2)

    if (sum(abs(exDemTime2)) < 0.0001 .or. dynFlag == 1) then
        bar = 2
    end if

    itersTime = itersTime + 1

end do ! While loop

    ! --------------------------------------------------------------------
    !                           Write output
    ! --------------------------------------------------------------------
    
    
do r = 1, 5
    write(20,*) " "
end do
write(20,*) "-------------------------------------------------------------------------------------------------"
write(20,*) "                                Output From Simulation (Economic variables)"
write(20,*) "-------------------------------------------------------------------------------------------------"
do r = 1, 5
    write(20,*) " "
end do

! ---------------------------------------- Algorithm Variables

write(*,*) "Exit code:" , foo
write(20,*) "Exit code:" , foo
write(20,*) "SSE (Economy) :", sum(exDem**2)
write(20,*) "SSE (Trade) :", tradeAcc
write(20,*) "Minpoint:", minpoint
write(20,*) "Iterations:", iters
write(20,*) "iv = ", myiv   ! Write out the initial vertex

! ---------------------------------------- Economic Variables
write(20,*) " Comm Prices Net:"
do tt = 1,T
    do ii = 1,N
        write (20,*) p1(:,ii,tt)
    end do
end do

write(20,*) " Comm Prices Gross:"
do tt = 1,T
    do ii = 1,N
        write (20,*) p2(:,ii,tt)
    end do
end do
do tt = 1,T
    write(20,*) " TR:", TR(:,tt)
    write(20,*) " RV:", RV(:,tt)
end do
write(20,*) " Excess Demands:"
write(20,*) exDem
write(20,*) " Primitive Simplex:"
do ii = 1,Q
    write(20,*) myps(ii,:)
end do

do ii = 1,N
    write(20,*) "Region", ii
    write(20,*) "Int Rates", iRate(ii,:) * 100
    write(20,*) "Income   ", incPath(ii,:)
    write(20,*) "Consume  ", consPath(ii,:)
    write(20,*) "Invest   ", invPath(ii,:)
    write(20,*) "Capital 1", capPath1(ii,:)
    write(20,*) "Capital 2", capPath2(ii,:)
    write(20,*) "Ex Dem   ", exDemTime2(ii,:)
end do

do tt = 1,T
do ii = 1,N
    
    write(20,*) "--------------------","Period", tt," Country", ii, " -------------------"
    write(20,*) "Cons Dem.    :", consDemand(:,ii,tt)
    write(20,*) "Final Demand :", finalDemand(:,ii,tt)
    write(20,*) "Intermediate :", interDemand(:,ii,tt)
    write(20,*) "Labor Supply :", laborSupply(:,ii,tt)
    write(20,*) "Cap. Supply  :", capitalSupply(:,ii,tt)
    write(20,*) "Wages        :", wage(:,ii,tt)
    write(20,*) "Labor per Q  :", labDem(:,ii,tt)
    write(20,*) "Cap per Q    :", capDem(:,ii,tt)
    write(20,*) "VA Share     :", labDem(:,ii,tt) + capDem(:,ii,tt)
    write(20,*) "Tot Lab Dem  :", totLabDem(:,ii,tt)
    write(20,*) "Tot Cap Dem  :", totCapDem(:,ii,tt)
    write(20,*) "Labor Alloc  :", labAlloc(:,ii,tt)
    write(20,*) "Comm. Demand :", commDemand(:,ii,tt)
    write(20,*) "Comm. Supply :", commSupply(:,ii,tt)
    write(20,*) "Dem. Share   :", shareD(:,ii,tt)
    write(20,*) "Sup. Share   :", shareS(:,ii,tt)
    write(20,*) "Excess Supply:", exSupply(:,ii,tt)
    write(20,*) "Trf Tax Rev  :", taxRevTariff(:,ii,tt)
    write(20,*) "Subsidy Rev  :", taxRevSubsidy(:,ii,tt)
    write(20,*) "Tariffs      :", p3(:,ii,tt)
    write(20,*) "Emissions    :",emissions(:,ii,tt), "total :", sum(emissions(:,ii,tt))
    write(20,*) "Dividend     :", dividend(:,ii,tt)
    write(20,*) "Int. Trade   :", itcost1(:,ii,tt)
    write(20,*) "Shares       :"
    do r = 1,B
        write(20,*) share(r,:,ii,tt)
    end do

end do
end do

! Write out transportation output (trade flows)

do r = 1, 5
    write(20,*) " "
end do
write(20,*) "-------------------------------------------------------------------------------------------------"
write(20,*) "                                Output From Simulation (Transportation)"
write(20,*) "-------------------------------------------------------------------------------------------------"
do r = 1, 5
    write(20,*) " "
end do

do tt = 1,T
    write(20,*) " ------ Trade Flows in Period",tt,"-----"
    do r = 1,B
        do ii = 1,N
            write(20,*) tflow1(r,ii,:,tt)
        end do
    end do
end do

! Write out io matrix (trade flows)

do r = 1, 5
    write(20,*) " "
end do
write(20,*) "-------------------------------------------------------------------------------------------------"
write(20,*) "                                Output From Simulation (Transportation)"
write(20,*) "-------------------------------------------------------------------------------------------------"
do r = 1, 5
    write(20,*) " "
end do

do tt = 1,T
    write(20,*) " ------ Production Process in Period",tt,"-----"
    do ii = 1,N
        write(20,*) "--------------- *Region",ii
        do r = 1,B
            write(20,*) io(r,:,ii,tt)
        end do
    end do
end do


close(10)
close(20)

!!! --------------------------------------------------------------- Subroutines

contains

!subroutine writeout(matrix, n, m)
!    integer, parameter :: n, m, i
!    real, dimension(n,m)
    
!end subroutine writeout

subroutine cdProdDemand(q1, q2, p1, p2, alpha,share)
! p1 = rent
! q1 = capital
! p2 = wage
! q2 = labor

double precision q1, q2, alpha, p1, p2, share

q1 = ((( alpha * p1) / ((1-alpha)*p2))**(alpha-1))*share
q2 = ((( alpha * p1) / ((1-alpha)*p2))**(alpha))*share

end subroutine cdProdDemand

!!! Consumer demand subroutine

subroutine cdDemand(q, p, alpha, M)

double precision q, p, alpha, M

q = (alpha/p) * M

end subroutine cdDemand


!!!  Merrill subroutine

subroutine merrill( jout, ps, labels, iv, obj, start, v, verbose, refinement)

    ! This subroutine calculates the correct labels and replacement operations
    ! for Merrill's algorithm to solve a v-variable system of equations.
    ! Based off Merrill's algorithm which is a variant of Herb Scarf's 
    ! fixed point iteration algorithm.
    
    ! jout
    ! ps ...
    ! labels ...
    ! iv ...
    ! obj ...
    ! start = 1 if starting primitive simplex
    ! v ....

    integer v, start, jout, c, r, ind, p, i2, x1, major, refinement
    integer xt, jout1, jout2, i3, verbose
    double precision, dimension( v+1, v+1) :: ps
    integer, dimension(v+1) :: labels
    double precision, dimension(v) :: obj, iv 
    
    if(jout > v+1)then
        jout = 1
        write(*,*) "Err. in merrill subroutine:"
        write(*,*) "jout out of bounds - resetting to 1"
    end if
    
    ! Minor iteration
    
    ! Find duplicate label
    
    ! If the algorithm has exited and started again - i.e.
    ! the subroutine is called in the program then we know
    ! we are on the artificial simplex - use Scarf labeling
    ! Find first entry in objective vector that is positive
    ind = 0
    do r = 0, v-1
        if (ps(v+1-r,jout) == 0) then
            ind = v-r
            exit
        else if (obj(v-r)>0) then
            ind = v-r ! Index of positive entry
        end if
    end do
    
    labels(jout) = ind ! Set the label
    
    ! Write out simplex for troubleshooting
    if (verbose == 1) then
        write(*,*) "----------Prim. Simplex---------"
        do r = 1,v+1
            write(*,*) ps(r,:)
        end do
        write(*,*) "--------Labeling ", jout," as ", ind, "--------"
        write(*,*) "Labels: ", labels
    end if    
    
    ! Now we that we have the correct labels we can go
    ! back into the original algorithm. Remember the
    ! outside program is just generating the excess demands
    ! or objective function values. Once we have calculated
    ! excess demands we feed it back into the subroutine 'merrill'
    
    ! Exit flag -- This means we have a simplex that is either
    ! comletely labeled or
    ! in need of excess demands to be calculated
    major = 0
    xt = 0
    
    if (start == 1) then
        xt = 1
        major = 1
    end if
    
    do while (xt == 0)
    
    
        ! See if simplex is completely labeled
        x1 = 0
        if ( sum(ps(1,:)) == 1 ) then
            x1 = 1 ! First condition
        end if
        
        
        ! We have a ps with only one vertex (x1) on the artificial plane
        ! now we need to see if the simplex on the real plane is
        p = 0
        if (x1 == 1) then
            do i2 = 1, v
                do i3 = 1, v + 1
                    ! Second condition below here
                    if ( labels ( i3 ) == i2 .and. ps ( 1, i3 ) == 0 ) then
                        p = p + 1
                        exit
                    end if
                end do
            end do
        end if
        
        if (p == v .and. x1 == 1) then
            ! Simplex is completely labeled - move to major iteration
            !write(*,*) "--------------------------------------"
            !write(*,*) "            Major Iteration", sum(abs(obj))
            !write(*,*) "--------------------------------------"
            major = 1
            xt = 1
            do i2 = 2, v+1
                iv(i2-1) = ps(i2,jout)*refinement
                !write(*,*) "Vertex: ", iv(i2-1), "Value: ", obj(i2-1)
            end do
        end if
        
        
        ! If simplex is not completely labeled
        ! We need to replace the duplicate column
        do r = 1, v + 1
            if (labels(jout) == labels(r) .and. jout /= r) then
                jout = r ! Index of duplicate label
                exit
            end if
        end do
        
        if(verbose == 1)then
            write(*,*) "Replacing column ", jout
        end if
        
        ! Now we make the replacement
            jout1 = jout - 1
            jout2 = jout + 1
            if ( jout1 == 0 ) then
                jout1 = v + 1
            end if
            if ( jout2 > ( v + 1 ) ) then
                jout2 = 1
            end if
            
            ! replacement operation
            ps( :, jout ) = ps( :, jout2 ) + ps( :, jout1 ) - ps( :, jout )
            
            ! If we are on the artificial simplex calculate label
            if (ps(1,jout) == 1) then
                do r = 1, v
                    if (ps(r+1,jout)<iv(r)) then
                        labels(jout) = r
                        exit
                    end if
                end do
            else
                xt = 1
            end if
            
                            ! Write out simplex for troubleshooting
            if (verbose == 1) then
                write(*,*) "----------New Prim. Simplex---------"
                do r = 1,v+1
                    write(*,*) ps(r,:)
                end do
                write(*,*) "Labels: ", labels
            end if  
            
        
    end do
    
    
    
    ! ------------------------------------------------------------ Major iteration
    
    ! If this is the major iteration (or first time through) we
    ! need to setup the new primitive simplex from the initial
    ! vertex, as well as initialize the labeling vector.
    
    if (major == 1) then
    
        do c = 1, v
        
            labels(c+1) = c ! Setup Labels
            
            do r = 1, v
            
                ps( r+1, c+1) = iv(r) ! Setup primitive simplex
                
                if (c == r) then
                    ps( r+1, c+1) = iv(r) - 1 ! Diagonal entry shifted
                end if
            
            end do    
            
        end do
        
        
        do r = 1, v
            ps( r+1, 1) = iv(r) ! Set first column up as initial vertex
            ps( 1, r+1) = 1 ! Set first row as artifial simplex
            labels(r+1) = r
        end do
        
        ps(1,1) = 0
        jout = 1
    
    end if

    if(verbose == 1)then
        write(*,*) "-----------------"
        write(*,*) "Exiting Merrill"
        write(*,*) "Jout = ", jout
        write(*,*) "-----------------"
    end if
    
end subroutine merrill

! --------------------------------------------------------------------------------------------------------- Solve transportation polytope routine


subroutine tpout(m1, m2, calib, b1, N)

    integer N, i, j, mc, q
    ! mc is the minimum column
    integer, dimension(N) :: label, replace
    double precision mcval, d1
    double precision, dimension(N) :: m1, m2, xp, rsum
    double precision, dimension(N,N) :: calib, b1, b2
    
    ! m1 = rowSums, m2 = colSums
    
    ! | x x x x | D   Calib matrix needs to be
    ! | x x x x |     imported such that rows
    ! | x x x x |     sum to one.
    ! | x x x x |
    !  P 
    
    !Create base matrix
    do i = 1, N
        do j = 1, N
            b1(i,j) = m1(i) * calib(i,j)
        end do
    end do
    
    replace = 0

do q = 1, N-1

    ! Creat excess production
    xp = 0
    do j = 1, N
        do i = 1, N
            xp(j) = xp(j) + b1(i,j)
        end do
        xp(j) = xp(j) - m2(j)
        if ( xp(j) < 0 .and. replace(j) == 0) then
            label(j) = 1
        end if
    end do    
    
        ! Find max
    mcval = 0
    do j = 1,N
        if (xp(j) > mcval) then
            mc = j          ! Column we're removing
            mcval = xp(j)   ! How much
            replace(j) = 1  ! Mark it replaced
        end if
    end do
    
    ! Remove column
    rsum = 0.
    d1 = 0.
    b2 = b1
    do i = 1, N
    
        ! Remove column with excess production
        b2(i,mc) = b2(i,mc) - mcval * b1(i,mc)/sum(b1(:,mc))
        rsum(i) = mcval * b1(i,mc)/sum(b1(:,mc))      
        
    end do
    
    do i = 1, N
        
        d1 = 0.
        do j = 1, N
            if( label(j) == 1 ) then
                d1 = d1 + calib(i,j)
            end if
        end do
        
        do j = 1, N
            if( label(j) == 1) then
                b2(i,j) = b2(i,j) + rsum(i) * calib(i,j)/d1
            end if
        end do
        
    end do
    
 
    b1 = b2

end do    

end subroutine tpout


subroutine time3solve( r1, inc, beta, dep, cons, inv, cap, NN)
    
    integer NN
    double precision, dimension(NN) :: r1, r2, inc, cons, inv, cap
    double precision beta, dep, a1, a2, a3, a4
    
    
    ! Solve for optimal path of capital
    
    r2 = 1 - dep + r1
    cap(1) = 1
    
    a1 = ( beta*r2(1)*inc(2)-inc(3) ) / ( (1+beta)*r2(3) - 1 )
    a2 = ( beta*r2(3)*r2(2) ) / ( (1+beta)*r2(3) - 1 )
    a3 = ( beta*r2(3)*(r2(3)*cap(1)+inc(1)) ) - inc(2) + a1
    a4 = (1+beta)*r2(2) - a2
    
    cap(2) = a3/a4
    
    a1 = beta*r2(3)*r2(2)*cap(2) + beta*r2(3)*inc(2) - inc(3)
    a2 = (1+beta)*r2(3) - 1
    
    cap(3) = a1/a2
    
    cons(1) = r1(1)*cap(1) + inc(1) + (1-dep)*cap(1) - cap(2)
    cons(2) = r1(2)*cap(2) + inc(2) + (1-dep)*cap(2) - cap(3)
    cons(3) = r1(3)*cap(3) + inc(3) + (1-dep)*cap(3) - cap(3)
    inv(1) = r1(1)*cap(1) + inc(1) - cons(1)
    inv(2) = r1(2)*cap(2) + inc(2) - cons(2)
    inv(3) = r1(3)*cap(3) + inc(3) - cons(3)
    
    a1 = cons(1) + inv(1)
    a2 = cons(2) + inv(2)
    a3 = cons(3) + inv(3)
    
    
end subroutine time3solve

end program nonarmington
