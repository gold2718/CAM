      module mo_indprd
      use shr_kind_mod, only : r8 => shr_kind_r8
      private
      public :: indprd
      contains
      subroutine indprd( class, prod, nprod, y, extfrc, rxt, chnkpnts )
      use chem_mods, only : gas_pcnst, extcnt, rxntot
      implicit none
!--------------------------------------------------------------------
! ... dummy arguments
!--------------------------------------------------------------------
      integer, intent(in) :: class
      integer, intent(in) :: chnkpnts
      integer, intent(in) :: nprod
      real(r8), intent(in) :: y(chnkpnts,gas_pcnst)
      real(r8), intent(in) :: rxt(chnkpnts,rxntot)
      real(r8), intent(in) :: extfrc(chnkpnts,extcnt)
      real(r8), intent(inout) :: prod(chnkpnts,nprod)
!--------------------------------------------------------------------
! ... "independent" production for Explicit species
!--------------------------------------------------------------------
      if( class == 1 ) then
         prod(:,1) = + extfrc(:,12)
         prod(:,2) = 0._r8
         prod(:,3) = 0._r8
         prod(:,4) = 0._r8
         prod(:,5) = 0._r8
         prod(:,6) = 0._r8
         prod(:,7) = 0._r8
         prod(:,8) = 0._r8
         prod(:,9) = 0._r8
         prod(:,10) = 0._r8
         prod(:,11) = 0._r8
         prod(:,12) = 0._r8
         prod(:,13) = 0._r8
         prod(:,14) = 0._r8
         prod(:,15) =.100_r8*rxt(:,298)*y(:,144)*y(:,50)
         prod(:,16) = 0._r8
         prod(:,17) = 0._r8
         prod(:,18) = (rxt(:,255)*y(:,83) +rxt(:,257)*y(:,105) +rxt(:,265)*y(:,83) + &
                 rxt(:,285)*y(:,71) +.500_r8*rxt(:,286)*y(:,72) + &
                 .800_r8*rxt(:,291)*y(:,92) +rxt(:,292)*y(:,93) + &
                 .500_r8*rxt(:,341)*y(:,126) +1.800_r8*rxt(:,451)*y(:,166))*y(:,200) &
                  + (2.000_r8*rxt(:,281)*y(:,183) +.900_r8*rxt(:,282)*y(:,184) + &
                 rxt(:,284)*y(:,138) +2.000_r8*rxt(:,331)*y(:,195) + &
                 rxt(:,355)*y(:,191) +rxt(:,380)*y(:,207))*y(:,183) &
                  + (.200_r8*rxt(:,298)*y(:,50) +.100_r8*rxt(:,342)*y(:,128) + &
                 .270_r8*rxt(:,430)*y(:,27) +.270_r8*rxt(:,433)*y(:,127))*y(:,144) &
                  + (rxt(:,332)*y(:,184) +.450_r8*rxt(:,333)*y(:,189) + &
                 2.000_r8*rxt(:,334)*y(:,195))*y(:,195) &
                  + (.500_r8*rxt(:,440)*y(:,184) +.900_r8*rxt(:,442)*y(:,138)) &
                 *y(:,204) +rxt(:,37)*y(:,72) +.400_r8*rxt(:,60)*y(:,148) +rxt(:,65) &
                 *y(:,162) +.800_r8*rxt(:,69)*y(:,166)
         prod(:,19) = 0._r8
         prod(:,20) = 0._r8
         prod(:,21) = 0._r8
         prod(:,22) = 0._r8
         prod(:,23) = 0._r8
         prod(:,24) =rxt(:,141)*y(:,139)*y(:,129)
         prod(:,25) = 0._r8
         prod(:,26) = 0._r8
         prod(:,27) = 0._r8
         prod(:,28) = 0._r8
         prod(:,29) =rxt(:,468)*y(:,200)*y(:,134) +rxt(:,477)*y(:,135)
         prod(:,30) = (rxt(:,402)*y(:,185) +rxt(:,405)*y(:,194) +rxt(:,408)*y(:,196) + &
                 rxt(:,412)*y(:,150))*y(:,139) +.500_r8*rxt(:,341)*y(:,200)*y(:,126) &
                  +.200_r8*rxt(:,437)*y(:,198)*y(:,138) +.500_r8*rxt(:,449)*y(:,165) &
                 *y(:,140)
!--------------------------------------------------------------------
! ... "independent" production for Implicit species
!--------------------------------------------------------------------
      else if( class == 4 ) then
         prod(:,1) = 0._r8
         prod(:,2) = 0._r8
         prod(:,3) = 0._r8
         prod(:,4) = 0._r8
         prod(:,5) = + extfrc(:,5)
         prod(:,6) = + extfrc(:,3)
         prod(:,7) = + extfrc(:,2)
         prod(:,8) = + extfrc(:,1)
         prod(:,9) = 0._r8
         prod(:,10) = 0._r8
         prod(:,11) = 0._r8
         prod(:,12) = + extfrc(:,4)
         prod(:,13) = 0._r8
         prod(:,14) = 0._r8
         prod(:,15) = 0._r8
         prod(:,16) = 0._r8
         prod(:,17) = 0._r8
         prod(:,18) = 0._r8
         prod(:,19) = 0._r8
         prod(:,20) = 0._r8
         prod(:,21) = 0._r8
         prod(:,22) = 0._r8
         prod(:,23) = 0._r8
         prod(:,108) = 0._r8
         prod(:,110) = 0._r8
         prod(:,134) = 0._r8
         prod(:,35) = 0._r8
         prod(:,77) = 0._r8
         prod(:,36) = 0._r8
         prod(:,71) = 0._r8
         prod(:,85) = 0._r8
         prod(:,58) = 0._r8
         prod(:,103) = 0._r8
         prod(:,61) = 0._r8
         prod(:,50) = 0._r8
         prod(:,70) = 0._r8
         prod(:,163) =rxt(:,79)*y(:,55) +rxt(:,80)*y(:,56) +2.000_r8*rxt(:,86)*y(:,62) &
                  +rxt(:,87)*y(:,64) +3.000_r8*rxt(:,90)*y(:,76) +2.000_r8*rxt(:,98) &
                 *y(:,96)
         prod(:,46) = 0._r8
         prod(:,179) = 0._r8
         prod(:,99) = 0._r8
         prod(:,47) = 0._r8
         prod(:,68) = 0._r8
         prod(:,60) = 0._r8
         prod(:,100) = 0._r8
         prod(:,54) = 0._r8
         prod(:,62) = 0._r8
         prod(:,56) = 0._r8
         prod(:,139) = 0._r8
         prod(:,73) = 0._r8
         prod(:,28) = 0._r8
         prod(:,52) = 0._r8
         prod(:,176) =.180_r8*rxt(:,40)*y(:,75)
         prod(:,150) = 0._r8
         prod(:,25) = 0._r8
         prod(:,136) = 0._r8
         prod(:,155) = 0._r8
         prod(:,97) = 0._r8
         prod(:,91) = 0._r8
         prod(:,124) = 0._r8
         prod(:,80) = 0._r8
         prod(:,171) =4.000_r8*rxt(:,78)*y(:,54) +rxt(:,79)*y(:,55) &
                  +2.000_r8*rxt(:,81)*y(:,57) +2.000_r8*rxt(:,82)*y(:,58) &
                  +2.000_r8*rxt(:,83)*y(:,59) +rxt(:,84)*y(:,60) +2.000_r8*rxt(:,85) &
                 *y(:,61) +3.000_r8*rxt(:,88)*y(:,65) +rxt(:,89)*y(:,67) +rxt(:,100) &
                 *y(:,100) +rxt(:,101)*y(:,101) +rxt(:,102)*y(:,102)
         prod(:,34) = 0._r8
         prod(:,26) = 0._r8
         prod(:,172) = 0._r8
         prod(:,137) = 0._r8
         prod(:,144) =.380_r8*rxt(:,40)*y(:,75) +rxt(:,41)*y(:,84) + extfrc(:,9)
         prod(:,29) =rxt(:,79)*y(:,55) +rxt(:,80)*y(:,56) +rxt(:,82)*y(:,58) &
                  +2.000_r8*rxt(:,83)*y(:,59) +2.000_r8*rxt(:,84)*y(:,60) +rxt(:,85) &
                 *y(:,61) +2.000_r8*rxt(:,98)*y(:,96) +rxt(:,101)*y(:,101) +rxt(:,102) &
                 *y(:,102)
         prod(:,38) =rxt(:,81)*y(:,57) +rxt(:,82)*y(:,58) +rxt(:,100)*y(:,100)
         prod(:,41) = 0._r8
         prod(:,57) = 0._r8
         prod(:,30) = 0._r8
         prod(:,121) =rxt(:,80)*y(:,56) +rxt(:,84)*y(:,60)
         prod(:,140) = 0._r8
         prod(:,131) = 0._r8
         prod(:,165) = (rxt(:,39) +.330_r8*rxt(:,40))*y(:,75)
         prod(:,151) =1.440_r8*rxt(:,40)*y(:,75)
         prod(:,102) = 0._r8
         prod(:,31) = 0._r8
         prod(:,125) = 0._r8
         prod(:,166) = 0._r8
         prod(:,39) = 0._r8
         prod(:,122) = 0._r8
         prod(:,48) = 0._r8
         prod(:,164) = 0._r8
         prod(:,72) = 0._r8
         prod(:,120) = 0._r8
         prod(:,127) = 0._r8
         prod(:,143) = 0._r8
         prod(:,49) = 0._r8
         prod(:,145) = 0._r8
         prod(:,63) = 0._r8
         prod(:,32) = 0._r8
         prod(:,128) = 0._r8
         prod(:,105) = 0._r8
         prod(:,95) = 0._r8
         prod(:,153) = 0._r8
         prod(:,74) = 0._r8
         prod(:,112) = 0._r8
         prod(:,154) = 0._r8
         prod(:,64) = 0._r8
         prod(:,92) = 0._r8
         prod(:,65) = 0._r8
         prod(:,96) = 0._r8
         prod(:,133) = 0._r8
         prod(:,158) = 0._r8
         prod(:,75) = + extfrc(:,11)
         prod(:,59) = 0._r8
         prod(:,76) = 0._r8
         prod(:,141) = 0._r8
         prod(:,27) = 0._r8
         prod(:,24) = 0._r8
         prod(:,177) = + extfrc(:,6)
         prod(:,174) = + extfrc(:,7)
         prod(:,175) = 0._r8
         prod(:,130) = 0._r8
         prod(:,78) = 0._r8
         prod(:,178) =.180_r8*rxt(:,40)*y(:,75) +rxt(:,41)*y(:,84) + (rxt(:,5) + &
                 2.000_r8*rxt(:,6))
         prod(:,173) = 0._r8
         prod(:,66) = 0._r8
         prod(:,69) = 0._r8
         prod(:,51) = 0._r8
         prod(:,86) = 0._r8
         prod(:,33) = 0._r8
         prod(:,87) = 0._r8
         prod(:,37) = 0._r8
         prod(:,67) = 0._r8
         prod(:,98) = 0._r8
         prod(:,79) = 0._r8
         prod(:,93) = 0._r8
         prod(:,156) = 0._r8
         prod(:,129) = + extfrc(:,8)
         prod(:,53) = 0._r8
         prod(:,42) = 0._r8
         prod(:,104) = 0._r8
         prod(:,106) = 0._r8
         prod(:,88) = 0._r8
         prod(:,138) = 0._r8
         prod(:,142) = 0._r8
         prod(:,109) = 0._r8
         prod(:,40) = 0._r8
         prod(:,43) = 0._r8
         prod(:,44) = 0._r8
         prod(:,113) = 0._r8
         prod(:,45) = 0._r8
         prod(:,81) = 0._r8
         prod(:,94) = 0._r8
         prod(:,135) = 0._r8
         prod(:,89) = 0._r8
         prod(:,82) = 0._r8
         prod(:,126) = 0._r8
         prod(:,123) = 0._r8
         prod(:,107) = 0._r8
         prod(:,162) = 0._r8
         prod(:,168) =rxt(:,87)*y(:,64) +rxt(:,89)*y(:,67) +rxt(:,39)*y(:,75)
         prod(:,118) = 0._r8
         prod(:,101) = 0._r8
         prod(:,55) = 0._r8
         prod(:,114) = 0._r8
         prod(:,167) = 0._r8
         prod(:,83) = 0._r8
         prod(:,157) = 0._r8
         prod(:,160) = 0._r8
         prod(:,159) = 0._r8
         prod(:,115) = 0._r8
         prod(:,161) = 0._r8
         prod(:,132) = 0._r8
         prod(:,111) = 0._r8
         prod(:,148) = 0._r8
         prod(:,169) =rxt(:,12)*y(:,130) +rxt(:,5)
         prod(:,170) =.330_r8*rxt(:,40)*y(:,75) + extfrc(:,10)
         prod(:,84) = 0._r8
         prod(:,119) = 0._r8
         prod(:,149) = 0._r8
         prod(:,147) = 0._r8
         prod(:,146) = 0._r8
         prod(:,116) = 0._r8
         prod(:,152) = 0._r8
         prod(:,117) = 0._r8
         prod(:,90) = 0._r8
         prod(:,180) =.050_r8*rxt(:,40)*y(:,75)
      end if
      end subroutine indprd
      end module mo_indprd
