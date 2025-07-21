module Plant_Pesticide_Processes
implicit none
    contains
    
    

!******************************************************************************************************************************
SUBROUTINE plant_pesticide_degradation
      use  constants_and_Variables, ONLY: plant_pesticide_degrade_rate,plant_volatilization_rate,foliar_formation_ratio_12, &
           foliar_formation_ratio_23,Foliar_degrade_loss,FOLPST,plant_volatilization_rate,Foliar_volatile_loss, &
           NCHEM,delt
        

      implicit none
!
!      !Determines amount of pesticide which disappears from plant surface by first order decay and volatilization.  
!      The variable PLDKRT is a pseudo first order decay rate which may include processes of volatilization, oxidation, photolysis, etc.
!      !also determines pesticide washed off during rainfall events.

      real     ::  ex1, ex2, ex3, r,term1, term2, term3, term4, term50,term6,term7,term8,term90,term100
      real     ::  foliar_pesticide_loss(3), fol_deg(3),  foliar_pest_initial(3)

 
     Foliar_Pest_initial = FOLPST      !Save initial masses to use later
     
     fol_deg =  plant_pesticide_degrade_rate + plant_volatilization_rate  !local rate, need to 

    
     
             !Parent degradation for time step
             ex1 = EXP((-fol_deg(1))*DELT)
             Foliar_Pesticide_Loss(1) =  FOLPST(1) - FOLPST(1)*ex1  !store for output
             Foliar_volatile_loss(1)  =  plant_volatilization_rate(1)/ (fol_deg(1))*Foliar_Pesticide_Loss(1)
             Foliar_degrade_loss(1)   =  Foliar_Pesticide_Loss(1) - Foliar_volatile_loss(1)
             FOLPST(1) = Foliar_Pest_initial(1)*ex1               
                   
             If (nchem == 2 .or. nchem == 3) then  !Daughter
                ex2 = EXP((-fol_deg(2))*DELT)
                If (fol_deg(2)==fol_deg(1)) then
                    r = delt*ex2
                else
                    r =    (ex1-ex2)/(fol_deg(2)-fol_deg(1)) 
                end if
                FOLPST(2) = foliar_pest_initial(1)*foliar_formation_ratio_12*fol_deg(1)*r  + foliar_pest_initial(2)*ex2      
             end if
             
             
             If (nchem == 3) then !grandaughter

                 
                 ex3 = EXP((-fol_deg(3))*DELT)
                 !*************************************************************************************************
                 !I cant find the limit for cases when degradation rates are equal k1=k2=k3 (but there is one, if someone can find it).  
                 !In the mean tine, I just slightly alter the rates to make them slightly unequal
                 
                 IF (fol_deg(2)==fol_deg(1)) then                     
                     fol_deg(2) = fol_deg(1) * 1.0001   
                     ex2 = EXP((-fol_deg(2))*DELT)
                     
                     if (fol_deg(1)==0.0) then !added this to prevent calculation prblems when both deg rates are zero 1/25/21
                        FOLPST(2) = 0.0  
                     else  
                        r = (ex1-ex2)/(fol_deg(2)-fol_deg(1)) 
                        FOLPST(2) = foliar_pest_initial(1)*foliar_formation_ratio_12*fol_deg(1)*r  + foliar_pest_initial(2)*ex2   
                     end if
                     
                 end if      
                 
                 IF (fol_deg(3)==fol_deg(1)) then
                     fol_deg(3)=fol_deg(1) * 0.9999
                     ex3 = EXP((-fol_deg(3))*DELT)
                 end if
                 
                 IF (fol_deg(2)==fol_deg(3)) then
                     
                     fol_deg(2)=fol_deg(3) * 1.0001
                     ex2 = EXP((-fol_deg(2))*DELT)
                                                 
                    if (fol_deg(1)==0.0) then !added this to prevent calculation prblems when both deg rates are zero 1/25/21
                        FOLPST(2) = 0.0  
                     else  
                        r = (ex1-ex2)/(fol_deg(2)-fol_deg(1)) 
                        FOLPST(2) = foliar_pest_initial(1)*foliar_formation_ratio_12*fol_deg(1)*r  + foliar_pest_initial(2)*ex2   
                     end if
                 end if
                !**********************************************************************************************************
                 
                if (fol_deg(2)==0.0) then !added this to prevent calculation prblems when both deg rates are zero 1/25/21
                        FOLPST(3) = 0.0  
                else  
                 
                term1 = fol_deg(1)*fol_deg(2)/(fol_deg(2)-fol_deg(1))
                term2 = foliar_formation_ratio_12*foliar_formation_ratio_23*Foliar_Pest_initial(1)
                term3 = (ex1-ex3)/(fol_deg(3)-fol_deg(1)) 
                term4 = (ex3-ex2)/(fol_deg(3)-fol_deg(2))
                term50 = term1*term2*(term3 + term4)
                term6 = fol_deg(2)/(fol_deg(3)-fol_deg(2))
                term7 = foliar_formation_ratio_23* Foliar_Pest_initial(2)
                term8 = ex2-ex3
                term90 = term6*term7*term8
                term100 = Foliar_Pest_initial(3)*ex3
              
                FOLPST(3) =term50 + term90 + term100
                end if
                
             end if
             
                
END SUBROUTINE plant_pesticide_degradation

    
    
    
    
   
    !*************************************************************************************************************
    !NEW SUBROUTINE TO HANDLE PESTICIDE washoff

    subroutine plant_pesticide_Washoff
      use  constants_and_Variables, ONLY:canopy_flow, plant_washoff_coeff, &     
      theta_zero,delx,theta_sat, FOLPST, soil_applied_washoff, number_washoff_nodes, nchem
        
      implicit none

      !Determines amount of pesticide which disappears from plant surface by first order decay.  The variable PLDKRT
      !is a pseudo first order decay rate which may include processes of volatilization, oxidation, photolysis, etc.
      !also determines pesticide washed off during rainfall events.

      INTEGER:: K
      REAL         incremental_pore_space(number_washoff_nodes)
      REAL         available_pore_space
      INTEGER      I
      real    :: foliar_washoff(3)
     
      
      If (canopy_flow < 10e-6) then  
        soil_applied_washoff = 0.0
        return
      end if
      
           

      
      
      available_pore_space =0.0          
        
     !Pesticide is distributed in proportion to the pore volume of the compartments
     !This is previously undocumented.

     incremental_pore_space = (theta_sat(1:number_washoff_nodes)-theta_zero(1:number_washoff_nodes))*DELX(1:number_washoff_nodes)  !Available pore space
     
     do I=1,number_washoff_nodes   !Add up available pore space to 2cm
        available_pore_space = available_pore_space + incremental_pore_space(I)                         
     end do
     
     soil_applied_washoff = 0.0
     do concurrent (k=1:nchem)
         foliar_washoff(k)   = FOLPST(K)*(1.0 - exp(-(plant_washoff_coeff(K)*canopy_flow)))   !pesticide removed from foliage by washoff
         FOLPST(K) = FOLPST(K)- foliar_washoff(k)
         soil_applied_washoff(K,1:number_washoff_nodes) = (incremental_pore_space/available_pore_space)*foliar_washoff(k) !pesticide applied by washoff
     end do


        
    END SUBROUTINE plant_pesticide_Washoff
    
    
    
        ! *** A a drop-in replacement for plant_pesticide_degradation
    subroutine foliardeg
        use  constants_and_Variables, ONLY: plant_pesticide_degrade_rate,plant_volatilization_rate,foliar_formation_ratio_12, &
               foliar_formation_ratio_23,Foliar_degrade_loss,FOLPST,Foliar_volatile_loss, NCHEM,delt
          
          real, dimension(3) :: oldpst, d, volfrac, degfrac, loss, cnst
          real :: u2, u3, v3 ! These are components of the eigenvectors which are not zero or one
          real :: a1, a2, c1, c2, c3
          
          d = -(plant_pesticide_degrade_rate + plant_volatilization_rate)
          degfrac = plant_pesticide_degrade_rate/(-d)
          oldpst = FOLPST
          if (d(1) >= 0.0) then ! if there's no degradation of chem1, there's no chem2 or chem3
            loss(1) = 0.0
            Foliar_degrade_loss(1) = 0.0
            Foliar_volatile_loss(1) = 0.0
          else if (nchem == 1) then
            FOLPST(1) = oldpst(1) * exp(d(1) * delt)
            loss(1) = oldpst(1) - FOLPST(1)
            Foliar_degrade_loss(1) = loss(1) * degfrac(1)
            Foliar_volatile_loss(1) = loss(1) - Foliar_degrade_loss(1)
          else if (nchem == 2) then
            if (d(1) == d(2)) then ! we'll have repeated eigenvalues, so slow down deg rate just a bit
              d(2) = d(2) * 0.999
            end if
            a1 = plant_pesticide_degrade_rate(1) * foliar_formation_ratio_12
            c1 = oldpst(1)
            c2 = oldpst(2) - oldpst(1)*a1/(d(1) - d(2))
            FOLPST(1) = c1*exp(d(1) * delt)
            FOLPST(2) = c1*a1/(d(1) - d(2)) * exp(d(1)*delt) + c2 * exp(d(2)*delt)
            loss(1:2) = oldpst(1:2) - FOLPST(1:2)
            Foliar_degrade_loss(1:2) = loss(1:2) * degfrac(1:2)
            Foliar_volatile_loss(1:2) = loss(1:2) - Foliar_degrade_loss(1:2)
          else if (nchem == 3) then
            if(d(1) == d(2) .and. d(2) == d(3)) then  ! Adjust if eigenvalues aren't unique
              d(2) = d(2) * 0.999
              d(3) = d(3) * 0.998
            else if (d(1) == d(2)) then
              d(2) = d(2) * 0.999
              if (d(2) == d(3)) then  ! this should not really happen
                d(2) = d(2) * 0.999  ! but if it does, adjust it again
              end if
            else if (d(1) == d(3)) then
              d(3) = d(3) * 0.999
              if (d(3) == d(2)) then ! again, this would be very unlikely
                d(3) = d(3) * 0.999
              end if
            end if
            
            a1 = plant_pesticide_degrade_rate(1) * foliar_formation_ratio_12
            a2 = plant_pesticide_degrade_rate(2) * foliar_formation_ratio_23
            u2 = a1/(d(1)-d(2))
            u3 = a1*a2/((d(1)-d(2))*(d(1)-d(3)))
            v3 = a2/(d(2)-d(3))
            c1 = oldpst(1)
            c2 = oldpst(2) - oldpst(2)*u2
            c3 = oldpst(3) - oldpst(1)*u3 - (oldpst(2) - oldpst(1)*u2)*v3
            FOLPST(1) = c1*exp(d(1)*delt)
            FOLPST(2) = c1*u2*exp(d(1)*delt) + c2*exp(d(2)*delt)
            FOLPST(3) = c1*u3*exp(d(1)*delt) + c2*v3*exp(d(2)*delt) + c3*exp(d(3)*delt)
            loss = oldpst - FOLPST
            Foliar_degrade_loss = loss * degfrac
            Foliar_volatile_loss = loss - Foliar_degrade_loss
          else ! there is a problem
              print *, "Number of chemicals not 1, 2, or 3 in foliardeg"
          end if
        end subroutine foliardeg
    
    
    end module Plant_Pesticide_Processes