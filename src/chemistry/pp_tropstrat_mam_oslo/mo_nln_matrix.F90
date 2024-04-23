      module mo_nln_matrix
      use shr_kind_mod, only : r8 => shr_kind_r8
      use chem_mods, only: veclen
      private
      public :: nlnmat
      contains
      subroutine nlnmat01( avec_len, mat, y, rxt )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k,154) = .029_r8*rxt(k,469)*y(k,200)
         mat(k,715) = .150_r8*rxt(k,504)*y(k,144)
         mat(k,1634) = .150_r8*rxt(k,504)*y(k,127)
         mat(k,1432) = .029_r8*rxt(k,469)*y(k,88)
         mat(k,155) = .114_r8*rxt(k,469)*y(k,200)
         mat(k,671) = .050_r8*rxt(k,509)*y(k,140) + .050_r8*rxt(k,507)*y(k,144) &
                      + .050_r8*rxt(k,508)*y(k,200)
         mat(k,716) = .150_r8*rxt(k,506)*y(k,140) + .150_r8*rxt(k,505)*y(k,200)
         mat(k,1732) = .050_r8*rxt(k,509)*y(k,116) + .150_r8*rxt(k,506)*y(k,127)
         mat(k,1635) = .050_r8*rxt(k,507)*y(k,116)
         mat(k,1433) = .114_r8*rxt(k,469)*y(k,88) + .050_r8*rxt(k,508)*y(k,116) &
                      + .150_r8*rxt(k,505)*y(k,127)
         mat(k,477) = -(rxt(k,346)*y(k,200))
         mat(k,1502) = -rxt(k,346)*y(k,24)
         mat(k,1833) = rxt(k,349)*y(k,177)
         mat(k,760) = rxt(k,349)*y(k,138)
         mat(k,501) = -(rxt(k,350)*y(k,200))
         mat(k,1504) = -rxt(k,350)*y(k,25)
         mat(k,761) = rxt(k,347)*y(k,189)
         mat(k,1298) = rxt(k,347)*y(k,177)
         mat(k,742) = -(rxt(k,429)*y(k,140) + rxt(k,430)*y(k,144) + rxt(k,431) &
                      *y(k,200))
         mat(k,1743) = -rxt(k,429)*y(k,27)
         mat(k,1649) = -rxt(k,430)*y(k,27)
         mat(k,1527) = -rxt(k,431)*y(k,27)
         mat(k,55) = -(rxt(k,388)*y(k,200))
         mat(k,1439) = -rxt(k,388)*y(k,28)
         mat(k,268) = -(rxt(k,391)*y(k,200))
         mat(k,1473) = -rxt(k,391)*y(k,29)
         mat(k,340) = rxt(k,389)*y(k,189)
         mat(k,1277) = rxt(k,389)*y(k,178)
         mat(k,56) = .120_r8*rxt(k,388)*y(k,200)
         mat(k,1440) = .120_r8*rxt(k,388)*y(k,28)
         mat(k,739) = .100_r8*rxt(k,430)*y(k,144)
         mat(k,718) = .100_r8*rxt(k,433)*y(k,144)
         mat(k,1637) = .100_r8*rxt(k,430)*y(k,27) + .100_r8*rxt(k,433)*y(k,127)
         mat(k,1821) = .500_r8*rxt(k,390)*y(k,178) + .200_r8*rxt(k,417)*y(k,206) &
                      + .060_r8*rxt(k,423)*y(k,208)
         mat(k,341) = .500_r8*rxt(k,390)*y(k,138)
         mat(k,558) = .200_r8*rxt(k,417)*y(k,138)
         mat(k,574) = .060_r8*rxt(k,423)*y(k,138)
         mat(k,1814) = .200_r8*rxt(k,417)*y(k,206) + .200_r8*rxt(k,423)*y(k,208)
         mat(k,557) = .200_r8*rxt(k,417)*y(k,138)
         mat(k,572) = .200_r8*rxt(k,423)*y(k,138)
         mat(k,1829) = .200_r8*rxt(k,417)*y(k,206) + .150_r8*rxt(k,423)*y(k,208)
         mat(k,559) = .200_r8*rxt(k,417)*y(k,138)
         mat(k,575) = .150_r8*rxt(k,423)*y(k,138)
         mat(k,1815) = .210_r8*rxt(k,423)*y(k,208)
         mat(k,573) = .210_r8*rxt(k,423)*y(k,138)
         mat(k,125) = -(rxt(k,351)*y(k,200))
         mat(k,1452) = -rxt(k,351)*y(k,36)
         mat(k,738) = .050_r8*rxt(k,430)*y(k,144)
         mat(k,717) = .050_r8*rxt(k,433)*y(k,144)
         mat(k,1636) = .050_r8*rxt(k,430)*y(k,27) + .050_r8*rxt(k,433)*y(k,127)
         mat(k,224) = -(rxt(k,317)*y(k,140) + rxt(k,318)*y(k,200))
         mat(k,1736) = -rxt(k,317)*y(k,37)
         mat(k,1467) = -rxt(k,318)*y(k,37)
         mat(k,1205) = -(rxt(k,200)*y(k,63) + rxt(k,201)*y(k,189) + rxt(k,202) &
                      *y(k,144))
         mat(k,1793) = -rxt(k,200)*y(k,38)
         mat(k,1340) = -rxt(k,201)*y(k,38)
         mat(k,1673) = -rxt(k,202)*y(k,38)
         mat(k,1925) = 4.000_r8*rxt(k,203)*y(k,40) + (rxt(k,204)+rxt(k,205))*y(k,80) &
                      + rxt(k,208)*y(k,138) + rxt(k,211)*y(k,143) + rxt(k,456) &
                      *y(k,157) + rxt(k,212)*y(k,200)
         mat(k,1616) = (rxt(k,204)+rxt(k,205))*y(k,40)
         mat(k,645) = rxt(k,213)*y(k,143) + rxt(k,219)*y(k,199) + rxt(k,214)*y(k,200)
         mat(k,1871) = rxt(k,208)*y(k,40)
         mat(k,1901) = rxt(k,211)*y(k,40) + rxt(k,213)*y(k,99)
         mat(k,1039) = rxt(k,456)*y(k,40)
         mat(k,1414) = rxt(k,219)*y(k,99)
         mat(k,1556) = rxt(k,212)*y(k,40) + rxt(k,214)*y(k,99)
         mat(k,1919) = rxt(k,206)*y(k,80)
         mat(k,1610) = rxt(k,206)*y(k,40)
         mat(k,1244) = (rxt(k,491)+rxt(k,496))*y(k,109)
         mat(k,607) = (rxt(k,491)+rxt(k,496))*y(k,103)
         mat(k,1941) = -(4._r8*rxt(k,203)*y(k,40) + (rxt(k,204) + rxt(k,205) + rxt(k,206) &
                      ) * y(k,80) + rxt(k,207)*y(k,189) + rxt(k,208)*y(k,138) &
                      + rxt(k,209)*y(k,139) + rxt(k,211)*y(k,143) + rxt(k,212) &
                      *y(k,200) + rxt(k,456)*y(k,157))
         mat(k,1632) = -(rxt(k,204) + rxt(k,205) + rxt(k,206)) * y(k,40)
         mat(k,1356) = -rxt(k,207)*y(k,40)
         mat(k,1887) = -rxt(k,208)*y(k,40)
         mat(k,1730) = -rxt(k,209)*y(k,40)
         mat(k,1917) = -rxt(k,211)*y(k,40)
         mat(k,1572) = -rxt(k,212)*y(k,40)
         mat(k,1049) = -rxt(k,456)*y(k,40)
         mat(k,1213) = rxt(k,202)*y(k,144)
         mat(k,416) = rxt(k,210)*y(k,143)
         mat(k,650) = rxt(k,220)*y(k,199)
         mat(k,614) = rxt(k,215)*y(k,143)
         mat(k,1917) = mat(k,1917) + rxt(k,210)*y(k,41) + rxt(k,215)*y(k,109)
         mat(k,1689) = rxt(k,202)*y(k,38)
         mat(k,1430) = rxt(k,220)*y(k,99)
         mat(k,409) = -(rxt(k,210)*y(k,143))
         mat(k,1891) = -rxt(k,210)*y(k,41)
         mat(k,1921) = rxt(k,209)*y(k,139)
         mat(k,1699) = rxt(k,209)*y(k,40)
         mat(k,114) = -(rxt(k,392)*y(k,200))
         mat(k,1450) = -rxt(k,392)*y(k,43)
         mat(k,1811) = rxt(k,395)*y(k,179)
         mat(k,298) = rxt(k,395)*y(k,138)
         mat(k,211) = -(rxt(k,394)*y(k,200))
         mat(k,1465) = -rxt(k,394)*y(k,44)
         mat(k,299) = rxt(k,393)*y(k,189)
         mat(k,1273) = rxt(k,393)*y(k,179)
         mat(k,171) = -(rxt(k,266)*y(k,77) + rxt(k,267)*y(k,200))
         mat(k,1576) = -rxt(k,266)*y(k,45)
         mat(k,1459) = -rxt(k,267)*y(k,45)
         mat(k,417) = -(rxt(k,268)*y(k,77) + rxt(k,269)*y(k,144) + rxt(k,294)*y(k,200))
         mat(k,1577) = -rxt(k,268)*y(k,46)
         mat(k,1641) = -rxt(k,269)*y(k,46)
         mat(k,1494) = -rxt(k,294)*y(k,46)
         mat(k,140) = -(rxt(k,274)*y(k,200))
         mat(k,1456) = -rxt(k,274)*y(k,47)
         mat(k,652) = .800_r8*rxt(k,270)*y(k,180) + .200_r8*rxt(k,271)*y(k,184)
         mat(k,1358) = .200_r8*rxt(k,271)*y(k,180)
         mat(k,182) = -(rxt(k,275)*y(k,200))
         mat(k,1460) = -rxt(k,275)*y(k,48)
         mat(k,653) = rxt(k,272)*y(k,189)
         mat(k,1269) = rxt(k,272)*y(k,180)
         mat(k,148) = -(rxt(k,276)*y(k,77) + rxt(k,277)*y(k,200))
         mat(k,1575) = -rxt(k,276)*y(k,49)
         mat(k,1457) = -rxt(k,277)*y(k,49)
         mat(k,804) = -(rxt(k,297)*y(k,140) + rxt(k,298)*y(k,144) + rxt(k,315) &
                      *y(k,200))
         mat(k,1747) = -rxt(k,297)*y(k,50)
         mat(k,1653) = -rxt(k,298)*y(k,50)
         mat(k,1532) = -rxt(k,315)*y(k,50)
         mat(k,674) = .130_r8*rxt(k,375)*y(k,144)
         mat(k,1653) = mat(k,1653) + .130_r8*rxt(k,375)*y(k,116)
         mat(k,244) = -(rxt(k,302)*y(k,200))
         mat(k,1469) = -rxt(k,302)*y(k,51)
         mat(k,629) = rxt(k,300)*y(k,189)
         mat(k,1275) = rxt(k,300)*y(k,181)
         mat(k,34) = -(rxt(k,303)*y(k,200))
         mat(k,1436) = -rxt(k,303)*y(k,52)
         mat(k,131) = -(rxt(k,398)*y(k,200))
         mat(k,1454) = -rxt(k,398)*y(k,53)
         mat(k,468) = rxt(k,396)*y(k,189)
         mat(k,1267) = rxt(k,396)*y(k,182)
         mat(k,1806) = -(rxt(k,164)*y(k,77) + rxt(k,200)*y(k,38) + rxt(k,244)*y(k,189) &
                      + rxt(k,245)*y(k,140) + rxt(k,246)*y(k,143) + rxt(k,247) &
                      *y(k,200))
         mat(k,1603) = -rxt(k,164)*y(k,63)
         mat(k,1211) = -rxt(k,200)*y(k,63)
         mat(k,1353) = -rxt(k,244)*y(k,63)
         mat(k,1783) = -rxt(k,245)*y(k,63)
         mat(k,1914) = -rxt(k,246)*y(k,63)
         mat(k,1569) = -rxt(k,247)*y(k,63)
         mat(k,486) = .400_r8*rxt(k,346)*y(k,200)
         mat(k,757) = .340_r8*rxt(k,430)*y(k,144)
         mat(k,231) = .500_r8*rxt(k,317)*y(k,140)
         mat(k,424) = rxt(k,269)*y(k,144)
         mat(k,816) = .500_r8*rxt(k,298)*y(k,144)
         mat(k,361) = .500_r8*rxt(k,286)*y(k,200)
         mat(k,643) = rxt(k,252)*y(k,200)
         mat(k,290) = .300_r8*rxt(k,253)*y(k,200)
         mat(k,1629) = rxt(k,171)*y(k,184)
         mat(k,824) = .800_r8*rxt(k,291)*y(k,200)
         mat(k,687) = .910_r8*rxt(k,375)*y(k,144)
         mat(k,461) = .300_r8*rxt(k,366)*y(k,200)
         mat(k,1011) = .800_r8*rxt(k,370)*y(k,184)
         mat(k,1024) = .120_r8*rxt(k,328)*y(k,144)
         mat(k,396) = .500_r8*rxt(k,341)*y(k,200)
         mat(k,736) = .340_r8*rxt(k,433)*y(k,144)
         mat(k,1096) = .600_r8*rxt(k,342)*y(k,144)
         mat(k,1884) = .100_r8*rxt(k,348)*y(k,177) + rxt(k,251)*y(k,184) &
                      + .500_r8*rxt(k,319)*y(k,186) + .500_r8*rxt(k,288)*y(k,188) &
                      + .920_r8*rxt(k,358)*y(k,191) + .250_r8*rxt(k,326)*y(k,193) &
                      + rxt(k,335)*y(k,195) + rxt(k,309)*y(k,202) + rxt(k,313) &
                      *y(k,203) + .340_r8*rxt(k,442)*y(k,204) + .320_r8*rxt(k,447) &
                      *y(k,205) + .250_r8*rxt(k,383)*y(k,207)
         mat(k,1783) = mat(k,1783) + .500_r8*rxt(k,317)*y(k,37) + rxt(k,359)*y(k,191) &
                      + .250_r8*rxt(k,325)*y(k,193) + rxt(k,336)*y(k,195)
         mat(k,1686) = .340_r8*rxt(k,430)*y(k,27) + rxt(k,269)*y(k,46) &
                      + .500_r8*rxt(k,298)*y(k,50) + .910_r8*rxt(k,375)*y(k,116) &
                      + .120_r8*rxt(k,328)*y(k,122) + .340_r8*rxt(k,433)*y(k,127) &
                      + .600_r8*rxt(k,342)*y(k,128)
         mat(k,328) = rxt(k,293)*y(k,200)
         mat(k,848) = .680_r8*rxt(k,451)*y(k,200)
         mat(k,772) = .100_r8*rxt(k,348)*y(k,138)
         mat(k,661) = .700_r8*rxt(k,271)*y(k,184)
         mat(k,637) = rxt(k,299)*y(k,184)
         mat(k,1200) = rxt(k,282)*y(k,184) + rxt(k,355)*y(k,191) + .250_r8*rxt(k,322) &
                      *y(k,193) + rxt(k,331)*y(k,195) + .250_r8*rxt(k,380)*y(k,207)
         mat(k,1403) = rxt(k,171)*y(k,80) + .800_r8*rxt(k,370)*y(k,119) + rxt(k,251) &
                      *y(k,138) + .700_r8*rxt(k,271)*y(k,180) + rxt(k,299)*y(k,181) &
                      + rxt(k,282)*y(k,183) + (4.000_r8*rxt(k,248)+2.000_r8*rxt(k,249)) &
                      *y(k,184) + 1.500_r8*rxt(k,356)*y(k,191) + .750_r8*rxt(k,361) &
                      *y(k,192) + .880_r8*rxt(k,323)*y(k,193) + 2.000_r8*rxt(k,332) &
                      *y(k,195) + .750_r8*rxt(k,435)*y(k,198) + .800_r8*rxt(k,311) &
                      *y(k,203) + .930_r8*rxt(k,440)*y(k,204) + .950_r8*rxt(k,445) &
                      *y(k,205) + .800_r8*rxt(k,381)*y(k,207)
         mat(k,431) = .500_r8*rxt(k,319)*y(k,138)
         mat(k,549) = .500_r8*rxt(k,288)*y(k,138)
         mat(k,1353) = mat(k,1353) + .450_r8*rxt(k,333)*y(k,195) + .150_r8*rxt(k,312) &
                      *y(k,203)
         mat(k,1076) = .920_r8*rxt(k,358)*y(k,138) + rxt(k,359)*y(k,140) + rxt(k,355) &
                      *y(k,183) + 1.500_r8*rxt(k,356)*y(k,184)
         mat(k,1152) = .750_r8*rxt(k,361)*y(k,184)
         mat(k,1118) = .250_r8*rxt(k,326)*y(k,138) + .250_r8*rxt(k,325)*y(k,140) &
                      + .250_r8*rxt(k,322)*y(k,183) + .880_r8*rxt(k,323)*y(k,184)
         mat(k,1170) = rxt(k,335)*y(k,138) + rxt(k,336)*y(k,140) + rxt(k,331)*y(k,183) &
                      + 2.000_r8*rxt(k,332)*y(k,184) + .450_r8*rxt(k,333)*y(k,189) &
                      + 4.000_r8*rxt(k,334)*y(k,195)
         mat(k,934) = .750_r8*rxt(k,435)*y(k,184)
         mat(k,1569) = mat(k,1569) + .400_r8*rxt(k,346)*y(k,24) + .500_r8*rxt(k,286) &
                      *y(k,72) + rxt(k,252)*y(k,73) + .300_r8*rxt(k,253)*y(k,74) &
                      + .800_r8*rxt(k,291)*y(k,92) + .300_r8*rxt(k,366)*y(k,117) &
                      + .500_r8*rxt(k,341)*y(k,126) + rxt(k,293)*y(k,148) &
                      + .680_r8*rxt(k,451)*y(k,166)
         mat(k,604) = rxt(k,309)*y(k,138)
         mat(k,947) = rxt(k,313)*y(k,138) + .800_r8*rxt(k,311)*y(k,184) &
                      + .150_r8*rxt(k,312)*y(k,189)
         mat(k,914) = .340_r8*rxt(k,442)*y(k,138) + .930_r8*rxt(k,440)*y(k,184)
         mat(k,895) = .320_r8*rxt(k,447)*y(k,138) + .950_r8*rxt(k,445)*y(k,184)
         mat(k,988) = .250_r8*rxt(k,383)*y(k,138) + .250_r8*rxt(k,380)*y(k,183) &
                      + .800_r8*rxt(k,381)*y(k,184)
      end do
      end subroutine nlnmat01
      subroutine nlnmat02( avec_len, mat, y, rxt )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k,951) = -(rxt(k,278)*y(k,140) + rxt(k,279)*y(k,200))
         mat(k,1758) = -rxt(k,278)*y(k,66)
         mat(k,1543) = -rxt(k,279)*y(k,66)
         mat(k,481) = .800_r8*rxt(k,346)*y(k,200)
         mat(k,227) = rxt(k,317)*y(k,140)
         mat(k,141) = rxt(k,274)*y(k,200)
         mat(k,184) = .500_r8*rxt(k,275)*y(k,200)
         mat(k,807) = .500_r8*rxt(k,298)*y(k,144)
         mat(k,1083) = .100_r8*rxt(k,342)*y(k,144)
         mat(k,1860) = .400_r8*rxt(k,348)*y(k,177) + rxt(k,273)*y(k,180) &
                      + .270_r8*rxt(k,301)*y(k,181) + rxt(k,319)*y(k,186) + rxt(k,338) &
                      *y(k,197) + rxt(k,309)*y(k,202)
         mat(k,1758) = mat(k,1758) + rxt(k,317)*y(k,37)
         mat(k,1662) = .500_r8*rxt(k,298)*y(k,50) + .100_r8*rxt(k,342)*y(k,128)
         mat(k,766) = .400_r8*rxt(k,348)*y(k,138)
         mat(k,656) = rxt(k,273)*y(k,138) + 3.200_r8*rxt(k,270)*y(k,180) &
                      + .800_r8*rxt(k,271)*y(k,184)
         mat(k,632) = .270_r8*rxt(k,301)*y(k,138)
         mat(k,1380) = .800_r8*rxt(k,271)*y(k,180)
         mat(k,428) = rxt(k,319)*y(k,138)
         mat(k,1328) = .200_r8*rxt(k,337)*y(k,197)
         mat(k,513) = rxt(k,338)*y(k,138) + .200_r8*rxt(k,337)*y(k,189)
         mat(k,1543) = mat(k,1543) + .800_r8*rxt(k,346)*y(k,24) + rxt(k,274)*y(k,47) &
                      + .500_r8*rxt(k,275)*y(k,48)
         mat(k,600) = rxt(k,309)*y(k,138)
         mat(k,25) = -(rxt(k,280)*y(k,200))
         mat(k,1434) = -rxt(k,280)*y(k,68)
         mat(k,774) = -(rxt(k,316)*y(k,200))
         mat(k,1529) = -rxt(k,316)*y(k,69)
         mat(k,480) = .800_r8*rxt(k,346)*y(k,200)
         mat(k,744) = .520_r8*rxt(k,430)*y(k,144)
         mat(k,226) = .500_r8*rxt(k,317)*y(k,140)
         mat(k,723) = .520_r8*rxt(k,433)*y(k,144)
         mat(k,1848) = .250_r8*rxt(k,348)*y(k,177) + .820_r8*rxt(k,301)*y(k,181) &
                      + .500_r8*rxt(k,319)*y(k,186) + .270_r8*rxt(k,442)*y(k,204) &
                      + .040_r8*rxt(k,447)*y(k,205)
         mat(k,1745) = .500_r8*rxt(k,317)*y(k,37)
         mat(k,1651) = .520_r8*rxt(k,430)*y(k,27) + .520_r8*rxt(k,433)*y(k,127)
         mat(k,840) = .500_r8*rxt(k,451)*y(k,200)
         mat(k,765) = .250_r8*rxt(k,348)*y(k,138)
         mat(k,631) = .820_r8*rxt(k,301)*y(k,138) + .820_r8*rxt(k,299)*y(k,184)
         mat(k,1369) = .820_r8*rxt(k,299)*y(k,181) + .150_r8*rxt(k,440)*y(k,204) &
                      + .025_r8*rxt(k,445)*y(k,205)
         mat(k,426) = .500_r8*rxt(k,319)*y(k,138)
         mat(k,1529) = mat(k,1529) + .800_r8*rxt(k,346)*y(k,24) + .500_r8*rxt(k,451) &
                      *y(k,166)
         mat(k,901) = .270_r8*rxt(k,442)*y(k,138) + .150_r8*rxt(k,440)*y(k,184)
         mat(k,879) = .040_r8*rxt(k,447)*y(k,138) + .025_r8*rxt(k,445)*y(k,184)
         mat(k,1027) = -(rxt(k,304)*y(k,140) + rxt(k,305)*y(k,200))
         mat(k,1762) = -rxt(k,304)*y(k,70)
         mat(k,1548) = -rxt(k,305)*y(k,70)
         mat(k,871) = rxt(k,306)*y(k,200)
         mat(k,1016) = .880_r8*rxt(k,328)*y(k,144)
         mat(k,1084) = .500_r8*rxt(k,342)*y(k,144)
         mat(k,1864) = .170_r8*rxt(k,401)*y(k,185) + .050_r8*rxt(k,364)*y(k,192) &
                      + .250_r8*rxt(k,326)*y(k,193) + .170_r8*rxt(k,407)*y(k,196) &
                      + .400_r8*rxt(k,417)*y(k,206) + .250_r8*rxt(k,383)*y(k,207) &
                      + .540_r8*rxt(k,423)*y(k,208) + .510_r8*rxt(k,426)*y(k,209)
         mat(k,1762) = mat(k,1762) + .050_r8*rxt(k,365)*y(k,192) + .250_r8*rxt(k,325) &
                      *y(k,193) + .250_r8*rxt(k,384)*y(k,207)
         mat(k,697) = rxt(k,307)*y(k,200)
         mat(k,1665) = .880_r8*rxt(k,328)*y(k,122) + .500_r8*rxt(k,342)*y(k,128)
         mat(k,1185) = .250_r8*rxt(k,322)*y(k,193) + .250_r8*rxt(k,380)*y(k,207)
         mat(k,1384) = .240_r8*rxt(k,323)*y(k,193) + .500_r8*rxt(k,311)*y(k,203) &
                      + .100_r8*rxt(k,381)*y(k,207)
         mat(k,591) = .170_r8*rxt(k,401)*y(k,138) + .070_r8*rxt(k,400)*y(k,189)
         mat(k,1333) = .070_r8*rxt(k,400)*y(k,185) + .070_r8*rxt(k,406)*y(k,196)
         mat(k,1138) = .050_r8*rxt(k,364)*y(k,138) + .050_r8*rxt(k,365)*y(k,140)
         mat(k,1107) = .250_r8*rxt(k,326)*y(k,138) + .250_r8*rxt(k,325)*y(k,140) &
                      + .250_r8*rxt(k,322)*y(k,183) + .240_r8*rxt(k,323)*y(k,184)
         mat(k,709) = .170_r8*rxt(k,407)*y(k,138) + .070_r8*rxt(k,406)*y(k,189)
         mat(k,1548) = mat(k,1548) + rxt(k,306)*y(k,113) + rxt(k,307)*y(k,141)
         mat(k,941) = .500_r8*rxt(k,311)*y(k,184)
         mat(k,567) = .400_r8*rxt(k,417)*y(k,138)
         mat(k,980) = .250_r8*rxt(k,383)*y(k,138) + .250_r8*rxt(k,384)*y(k,140) &
                      + .250_r8*rxt(k,380)*y(k,183) + .100_r8*rxt(k,381)*y(k,184)
         mat(k,583) = .540_r8*rxt(k,423)*y(k,138)
         mat(k,352) = .510_r8*rxt(k,426)*y(k,138)
         mat(k,397) = -(rxt(k,285)*y(k,200))
         mat(k,1492) = -rxt(k,285)*y(k,71)
         mat(k,800) = .120_r8*rxt(k,298)*y(k,144)
         mat(k,1640) = .120_r8*rxt(k,298)*y(k,50)
         mat(k,1176) = .100_r8*rxt(k,282)*y(k,184) + .150_r8*rxt(k,283)*y(k,189)
         mat(k,1362) = .100_r8*rxt(k,282)*y(k,183)
         mat(k,1292) = .150_r8*rxt(k,283)*y(k,183) + .150_r8*rxt(k,333)*y(k,195)
         mat(k,1157) = .150_r8*rxt(k,333)*y(k,189)
         mat(k,357) = -(rxt(k,286)*y(k,200))
         mat(k,1486) = -rxt(k,286)*y(k,72)
         mat(k,1175) = .400_r8*rxt(k,283)*y(k,189)
         mat(k,1290) = .400_r8*rxt(k,283)*y(k,183) + .400_r8*rxt(k,333)*y(k,195)
         mat(k,1155) = .400_r8*rxt(k,333)*y(k,189)
         mat(k,640) = -(rxt(k,252)*y(k,200))
         mat(k,1517) = -rxt(k,252)*y(k,73)
         mat(k,993) = .200_r8*rxt(k,370)*y(k,184)
         mat(k,654) = .300_r8*rxt(k,271)*y(k,184)
         mat(k,1365) = .200_r8*rxt(k,370)*y(k,119) + .300_r8*rxt(k,271)*y(k,180) &
                      + 2.000_r8*rxt(k,249)*y(k,184) + .250_r8*rxt(k,356)*y(k,191) &
                      + .250_r8*rxt(k,361)*y(k,192) + .250_r8*rxt(k,323)*y(k,193) &
                      + .250_r8*rxt(k,435)*y(k,198) + .500_r8*rxt(k,311)*y(k,203) &
                      + .250_r8*rxt(k,440)*y(k,204) + .250_r8*rxt(k,445)*y(k,205) &
                      + .300_r8*rxt(k,381)*y(k,207)
         mat(k,1053) = .250_r8*rxt(k,356)*y(k,184)
         mat(k,1126) = .250_r8*rxt(k,361)*y(k,184)
         mat(k,1100) = .250_r8*rxt(k,323)*y(k,184)
         mat(k,919) = .250_r8*rxt(k,435)*y(k,184)
         mat(k,938) = .500_r8*rxt(k,311)*y(k,184)
         mat(k,900) = .250_r8*rxt(k,440)*y(k,184)
         mat(k,878) = .250_r8*rxt(k,445)*y(k,184)
         mat(k,974) = .300_r8*rxt(k,381)*y(k,184)
         mat(k,286) = -(rxt(k,253)*y(k,200))
         mat(k,1476) = -rxt(k,253)*y(k,74)
         mat(k,1361) = rxt(k,250)*y(k,189)
         mat(k,1280) = rxt(k,250)*y(k,184)
         mat(k,1598) = -(rxt(k,164)*y(k,63) + rxt(k,166)*y(k,95) + rxt(k,167)*y(k,97) &
                      + (rxt(k,168) + rxt(k,169)) * y(k,189) + rxt(k,170)*y(k,144) &
                      + rxt(k,177)*y(k,81) + rxt(k,186)*y(k,110) + rxt(k,276)*y(k,49))
         mat(k,1801) = -rxt(k,164)*y(k,77)
         mat(k,969) = -rxt(k,166)*y(k,77)
         mat(k,437) = -rxt(k,167)*y(k,77)
         mat(k,1348) = -(rxt(k,168) + rxt(k,169)) * y(k,77)
         mat(k,1681) = -rxt(k,170)*y(k,77)
         mat(k,786) = -rxt(k,177)*y(k,77)
         mat(k,667) = -rxt(k,186)*y(k,77)
         mat(k,152) = -rxt(k,276)*y(k,77)
         mat(k,1933) = rxt(k,205)*y(k,80)
         mat(k,1624) = rxt(k,205)*y(k,40) + (4.000_r8*rxt(k,172)+2.000_r8*rxt(k,174)) &
                      *y(k,80) + rxt(k,176)*y(k,138) + rxt(k,181)*y(k,143) &
                      + rxt(k,457)*y(k,157) + rxt(k,171)*y(k,184) + rxt(k,182) &
                      *y(k,200)
         mat(k,73) = rxt(k,226)*y(k,199)
         mat(k,1256) = rxt(k,184)*y(k,143) + rxt(k,196)*y(k,199) + rxt(k,185)*y(k,200)
         mat(k,1879) = rxt(k,176)*y(k,80)
         mat(k,1909) = rxt(k,181)*y(k,80) + rxt(k,184)*y(k,103)
         mat(k,1043) = rxt(k,457)*y(k,80)
         mat(k,1398) = rxt(k,171)*y(k,80)
         mat(k,1422) = rxt(k,226)*y(k,86) + rxt(k,196)*y(k,103)
         mat(k,1564) = rxt(k,182)*y(k,80) + rxt(k,185)*y(k,103)
         mat(k,1574) = rxt(k,177)*y(k,81)
         mat(k,1609) = 2.000_r8*rxt(k,173)*y(k,80)
         mat(k,780) = rxt(k,177)*y(k,77) + (rxt(k,489)+rxt(k,494)+rxt(k,499))*y(k,103)
         mat(k,1243) = (rxt(k,489)+rxt(k,494)+rxt(k,499))*y(k,81) + (rxt(k,484) &
                       +rxt(k,490)+rxt(k,495))*y(k,110)
         mat(k,663) = (rxt(k,484)+rxt(k,490)+rxt(k,495))*y(k,103)
         mat(k,1608) = 2.000_r8*rxt(k,198)*y(k,80)
         mat(k,1625) = -(rxt(k,171)*y(k,184) + (4._r8*rxt(k,172) + 4._r8*rxt(k,173) &
                      + 4._r8*rxt(k,174) + 4._r8*rxt(k,198)) * y(k,80) + rxt(k,175) &
                      *y(k,189) + rxt(k,176)*y(k,138) + rxt(k,178)*y(k,139) + rxt(k,181) &
                      *y(k,143) + (rxt(k,182) + rxt(k,183)) * y(k,200) + (rxt(k,204) &
                      + rxt(k,205) + rxt(k,206)) * y(k,40) + rxt(k,457)*y(k,157))
         mat(k,1399) = -rxt(k,171)*y(k,80)
         mat(k,1349) = -rxt(k,175)*y(k,80)
         mat(k,1880) = -rxt(k,176)*y(k,80)
         mat(k,1723) = -rxt(k,178)*y(k,80)
         mat(k,1910) = -rxt(k,181)*y(k,80)
         mat(k,1565) = -(rxt(k,182) + rxt(k,183)) * y(k,80)
         mat(k,1934) = -(rxt(k,204) + rxt(k,205) + rxt(k,206)) * y(k,80)
         mat(k,1044) = -rxt(k,457)*y(k,80)
         mat(k,1599) = rxt(k,186)*y(k,110) + rxt(k,170)*y(k,144) + rxt(k,169)*y(k,189)
         mat(k,787) = rxt(k,179)*y(k,143)
         mat(k,1257) = rxt(k,197)*y(k,199)
         mat(k,668) = rxt(k,186)*y(k,77) + rxt(k,187)*y(k,143) + rxt(k,188)*y(k,200)
         mat(k,1910) = mat(k,1910) + rxt(k,179)*y(k,81) + rxt(k,187)*y(k,110)
         mat(k,1682) = rxt(k,170)*y(k,77)
         mat(k,203) = rxt(k,462)*y(k,157)
         mat(k,1044) = mat(k,1044) + rxt(k,462)*y(k,145)
         mat(k,1349) = mat(k,1349) + rxt(k,169)*y(k,77)
         mat(k,1423) = rxt(k,197)*y(k,103)
         mat(k,1565) = mat(k,1565) + rxt(k,188)*y(k,110)
         mat(k,782) = -(rxt(k,177)*y(k,77) + rxt(k,179)*y(k,143) + rxt(k,180)*y(k,200) &
                      + (rxt(k,489) + rxt(k,494) + rxt(k,499)) * y(k,103))
         mat(k,1584) = -rxt(k,177)*y(k,81)
         mat(k,1897) = -rxt(k,179)*y(k,81)
         mat(k,1530) = -rxt(k,180)*y(k,81)
         mat(k,1247) = -(rxt(k,489) + rxt(k,494) + rxt(k,499)) * y(k,81)
         mat(k,1614) = rxt(k,178)*y(k,139)
         mat(k,1707) = rxt(k,178)*y(k,80)
         mat(k,866) = -((rxt(k,255) + rxt(k,265)) * y(k,200))
         mat(k,1537) = -(rxt(k,255) + rxt(k,265)) * y(k,83)
         mat(k,747) = .230_r8*rxt(k,430)*y(k,144)
         mat(k,1204) = rxt(k,200)*y(k,63)
         mat(k,174) = .350_r8*rxt(k,267)*y(k,200)
         mat(k,420) = .630_r8*rxt(k,269)*y(k,144)
         mat(k,805) = .560_r8*rxt(k,298)*y(k,144)
         mat(k,1791) = rxt(k,200)*y(k,38) + rxt(k,164)*y(k,77) + rxt(k,245)*y(k,140) &
                      + rxt(k,246)*y(k,143) + rxt(k,247)*y(k,200)
         mat(k,1026) = rxt(k,304)*y(k,140) + rxt(k,305)*y(k,200)
         mat(k,1586) = rxt(k,164)*y(k,63)
         mat(k,703) = rxt(k,292)*y(k,200)
         mat(k,675) = .620_r8*rxt(k,375)*y(k,144)
         mat(k,1014) = .650_r8*rxt(k,328)*y(k,144)
         mat(k,726) = .230_r8*rxt(k,433)*y(k,144)
         mat(k,1081) = .560_r8*rxt(k,342)*y(k,144)
         mat(k,1854) = .170_r8*rxt(k,401)*y(k,185) + .220_r8*rxt(k,326)*y(k,193) &
                      + .400_r8*rxt(k,404)*y(k,194) + .350_r8*rxt(k,407)*y(k,196) &
                      + .225_r8*rxt(k,442)*y(k,204) + .250_r8*rxt(k,383)*y(k,207)
         mat(k,1752) = rxt(k,245)*y(k,63) + rxt(k,304)*y(k,70) + .220_r8*rxt(k,325) &
                      *y(k,193) + .500_r8*rxt(k,384)*y(k,207)
         mat(k,1898) = rxt(k,246)*y(k,63) + rxt(k,452)*y(k,146)
         mat(k,1656) = .230_r8*rxt(k,430)*y(k,27) + .630_r8*rxt(k,269)*y(k,46) &
                      + .560_r8*rxt(k,298)*y(k,50) + .620_r8*rxt(k,375)*y(k,116) &
                      + .650_r8*rxt(k,328)*y(k,122) + .230_r8*rxt(k,433)*y(k,127) &
                      + .560_r8*rxt(k,342)*y(k,128)
         mat(k,219) = rxt(k,452)*y(k,143) + rxt(k,453)*y(k,200)
         mat(k,842) = .700_r8*rxt(k,451)*y(k,200)
         mat(k,1180) = .220_r8*rxt(k,322)*y(k,193) + .250_r8*rxt(k,380)*y(k,207)
         mat(k,1374) = .110_r8*rxt(k,323)*y(k,193) + .125_r8*rxt(k,440)*y(k,204) &
                      + .200_r8*rxt(k,381)*y(k,207)
         mat(k,590) = .170_r8*rxt(k,401)*y(k,138) + .070_r8*rxt(k,400)*y(k,189)
         mat(k,1322) = .070_r8*rxt(k,400)*y(k,185) + .160_r8*rxt(k,403)*y(k,194) &
                      + .140_r8*rxt(k,406)*y(k,196)
         mat(k,1103) = .220_r8*rxt(k,326)*y(k,138) + .220_r8*rxt(k,325)*y(k,140) &
                      + .220_r8*rxt(k,322)*y(k,183) + .110_r8*rxt(k,323)*y(k,184)
         mat(k,553) = .400_r8*rxt(k,404)*y(k,138) + .160_r8*rxt(k,403)*y(k,189)
         mat(k,708) = .350_r8*rxt(k,407)*y(k,138) + .140_r8*rxt(k,406)*y(k,189)
         mat(k,1537) = mat(k,1537) + .350_r8*rxt(k,267)*y(k,45) + rxt(k,247)*y(k,63) &
                      + rxt(k,305)*y(k,70) + rxt(k,292)*y(k,93) + rxt(k,453)*y(k,146) &
                      + .700_r8*rxt(k,451)*y(k,166)
         mat(k,904) = .225_r8*rxt(k,442)*y(k,138) + .125_r8*rxt(k,440)*y(k,184)
         mat(k,977) = .250_r8*rxt(k,383)*y(k,138) + .500_r8*rxt(k,384)*y(k,140) &
                      + .250_r8*rxt(k,380)*y(k,183) + .200_r8*rxt(k,381)*y(k,184)
      end do
      end subroutine nlnmat02
      subroutine nlnmat03( avec_len, mat, y, rxt )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k,38) = -(rxt(k,225)*y(k,199))
         mat(k,1408) = -rxt(k,225)*y(k,85)
         mat(k,70) = -(rxt(k,226)*y(k,199))
         mat(k,1409) = -rxt(k,226)*y(k,86)
         mat(k,85) = -(rxt(k,399)*y(k,200))
         mat(k,1445) = -rxt(k,399)*y(k,87)
         mat(k,79) = .180_r8*rxt(k,419)*y(k,200)
         mat(k,1445) = mat(k,1445) + .180_r8*rxt(k,419)*y(k,168)
         mat(k,156) = -(rxt(k,466)*y(k,140) + (rxt(k,467) + rxt(k,469)) * y(k,200))
         mat(k,1733) = -rxt(k,466)*y(k,88)
         mat(k,1458) = -(rxt(k,467) + rxt(k,469)) * y(k,88)
         mat(k,542) = rxt(k,287)*y(k,189)
         mat(k,1265) = rxt(k,287)*y(k,188)
         mat(k,617) = -(rxt(k,222)*y(k,95) + rxt(k,223)*y(k,210) + rxt(k,224)*y(k,107))
         mat(k,961) = -rxt(k,222)*y(k,91)
         mat(k,1946) = -rxt(k,223)*y(k,91)
         mat(k,1216) = -rxt(k,224)*y(k,91)
         mat(k,39) = 2.000_r8*rxt(k,225)*y(k,199)
         mat(k,71) = rxt(k,226)*y(k,199)
         mat(k,1411) = 2.000_r8*rxt(k,225)*y(k,85) + rxt(k,226)*y(k,86)
         mat(k,820) = -(rxt(k,291)*y(k,200))
         mat(k,1533) = -rxt(k,291)*y(k,92)
         mat(k,454) = .700_r8*rxt(k,366)*y(k,200)
         mat(k,383) = .500_r8*rxt(k,367)*y(k,200)
         mat(k,264) = rxt(k,378)*y(k,200)
         mat(k,1850) = .050_r8*rxt(k,364)*y(k,192) + .530_r8*rxt(k,326)*y(k,193) &
                      + .225_r8*rxt(k,442)*y(k,204) + .250_r8*rxt(k,383)*y(k,207)
         mat(k,1748) = .050_r8*rxt(k,365)*y(k,192) + .530_r8*rxt(k,325)*y(k,193) &
                      + .250_r8*rxt(k,384)*y(k,207)
         mat(k,1178) = .530_r8*rxt(k,322)*y(k,193) + .250_r8*rxt(k,380)*y(k,207)
         mat(k,1371) = .260_r8*rxt(k,323)*y(k,193) + .125_r8*rxt(k,440)*y(k,204) &
                      + .100_r8*rxt(k,381)*y(k,207)
         mat(k,1130) = .050_r8*rxt(k,364)*y(k,138) + .050_r8*rxt(k,365)*y(k,140)
         mat(k,1101) = .530_r8*rxt(k,326)*y(k,138) + .530_r8*rxt(k,325)*y(k,140) &
                      + .530_r8*rxt(k,322)*y(k,183) + .260_r8*rxt(k,323)*y(k,184)
         mat(k,1533) = mat(k,1533) + .700_r8*rxt(k,366)*y(k,117) + .500_r8*rxt(k,367) &
                      *y(k,118) + rxt(k,378)*y(k,132)
         mat(k,902) = .225_r8*rxt(k,442)*y(k,138) + .125_r8*rxt(k,440)*y(k,184)
         mat(k,976) = .250_r8*rxt(k,383)*y(k,138) + .250_r8*rxt(k,384)*y(k,140) &
                      + .250_r8*rxt(k,380)*y(k,183) + .100_r8*rxt(k,381)*y(k,184)
         mat(k,702) = -(rxt(k,292)*y(k,200))
         mat(k,1524) = -rxt(k,292)*y(k,93)
         mat(k,173) = .650_r8*rxt(k,267)*y(k,200)
         mat(k,819) = .200_r8*rxt(k,291)*y(k,200)
         mat(k,827) = rxt(k,379)*y(k,200)
         mat(k,1845) = rxt(k,390)*y(k,178) + .050_r8*rxt(k,364)*y(k,192) &
                      + .400_r8*rxt(k,404)*y(k,194) + .170_r8*rxt(k,407)*y(k,196) &
                      + .700_r8*rxt(k,410)*y(k,201) + .600_r8*rxt(k,417)*y(k,206) &
                      + .250_r8*rxt(k,383)*y(k,207) + .340_r8*rxt(k,423)*y(k,208) &
                      + .170_r8*rxt(k,426)*y(k,209)
         mat(k,1741) = .050_r8*rxt(k,365)*y(k,192) + .250_r8*rxt(k,384)*y(k,207)
         mat(k,344) = rxt(k,390)*y(k,138)
         mat(k,1177) = .250_r8*rxt(k,380)*y(k,207)
         mat(k,1368) = .100_r8*rxt(k,381)*y(k,207)
         mat(k,1315) = .160_r8*rxt(k,403)*y(k,194) + .070_r8*rxt(k,406)*y(k,196)
         mat(k,1128) = .050_r8*rxt(k,364)*y(k,138) + .050_r8*rxt(k,365)*y(k,140)
         mat(k,552) = .400_r8*rxt(k,404)*y(k,138) + .160_r8*rxt(k,403)*y(k,189)
         mat(k,706) = .170_r8*rxt(k,407)*y(k,138) + .070_r8*rxt(k,406)*y(k,189)
         mat(k,1524) = mat(k,1524) + .650_r8*rxt(k,267)*y(k,45) + .200_r8*rxt(k,291) &
                      *y(k,92) + rxt(k,379)*y(k,133)
         mat(k,314) = .700_r8*rxt(k,410)*y(k,138)
         mat(k,564) = .600_r8*rxt(k,417)*y(k,138)
         mat(k,975) = .250_r8*rxt(k,383)*y(k,138) + .250_r8*rxt(k,384)*y(k,140) &
                      + .250_r8*rxt(k,380)*y(k,183) + .100_r8*rxt(k,381)*y(k,184)
         mat(k,580) = .340_r8*rxt(k,423)*y(k,138)
         mat(k,351) = .170_r8*rxt(k,426)*y(k,138)
         mat(k,1231) = -((rxt(k,124) + rxt(k,125) + rxt(k,126)) * y(k,189) + rxt(k,130) &
                      *y(k,144))
         mat(k,1342) = -(rxt(k,124) + rxt(k,125) + rxt(k,126)) * y(k,94)
         mat(k,1675) = -rxt(k,130)*y(k,94)
         mat(k,1795) = rxt(k,247)*y(k,200)
         mat(k,1592) = rxt(k,166)*y(k,95)
         mat(k,867) = rxt(k,265)*y(k,200)
         mat(k,620) = rxt(k,222)*y(k,95)
         mat(k,964) = rxt(k,166)*y(k,77) + rxt(k,222)*y(k,91) + rxt(k,122)*y(k,143) &
                      + rxt(k,114)*y(k,199) + rxt(k,131)*y(k,200)
         mat(k,646) = rxt(k,220)*y(k,199)
         mat(k,1250) = rxt(k,197)*y(k,199)
         mat(k,257) = rxt(k,152)*y(k,200)
         mat(k,1903) = rxt(k,122)*y(k,95) + rxt(k,134)*y(k,200)
         mat(k,221) = rxt(k,453)*y(k,200)
         mat(k,370) = rxt(k,458)*y(k,200)
         mat(k,1040) = rxt(k,463)*y(k,200)
         mat(k,1416) = rxt(k,114)*y(k,95) + rxt(k,220)*y(k,99) + rxt(k,197)*y(k,103)
         mat(k,1558) = rxt(k,247)*y(k,63) + rxt(k,265)*y(k,83) + rxt(k,131)*y(k,95) &
                      + rxt(k,152)*y(k,129) + rxt(k,134)*y(k,143) + rxt(k,453) &
                      *y(k,146) + rxt(k,458)*y(k,155) + rxt(k,463)*y(k,157)
         mat(k,962) = -(rxt(k,114)*y(k,199) + rxt(k,122)*y(k,143) + rxt(k,131) &
                      *y(k,200) + rxt(k,166)*y(k,77) + rxt(k,222)*y(k,91))
         mat(k,1413) = -rxt(k,114)*y(k,95)
         mat(k,1899) = -rxt(k,122)*y(k,95)
         mat(k,1544) = -rxt(k,131)*y(k,95)
         mat(k,1588) = -rxt(k,166)*y(k,95)
         mat(k,618) = -rxt(k,222)*y(k,95)
         mat(k,1229) = rxt(k,124)*y(k,189)
         mat(k,1329) = rxt(k,124)*y(k,94)
         mat(k,433) = -(rxt(k,123)*y(k,143) + rxt(k,132)*y(k,200) + rxt(k,167)*y(k,77))
         mat(k,1892) = -rxt(k,123)*y(k,97)
         mat(k,1496) = -rxt(k,132)*y(k,97)
         mat(k,1578) = -rxt(k,167)*y(k,97)
         mat(k,1294) = 2.000_r8*rxt(k,138)*y(k,189)
         mat(k,1496) = mat(k,1496) + 2.000_r8*rxt(k,137)*y(k,200)
         mat(k,135) = rxt(k,465)*y(k,210)
         mat(k,1943) = rxt(k,465)*y(k,159)
         mat(k,644) = -(rxt(k,213)*y(k,143) + rxt(k,214)*y(k,200) + (rxt(k,219) &
                      + rxt(k,220)) * y(k,199))
         mat(k,1894) = -rxt(k,213)*y(k,99)
         mat(k,1518) = -rxt(k,214)*y(k,99)
         mat(k,1412) = -(rxt(k,219) + rxt(k,220)) * y(k,99)
         mat(k,1203) = rxt(k,200)*y(k,63) + rxt(k,201)*y(k,189)
         mat(k,1790) = rxt(k,200)*y(k,38)
         mat(k,1311) = rxt(k,201)*y(k,38)
         mat(k,1251) = -(rxt(k,184)*y(k,143) + rxt(k,185)*y(k,200) + (rxt(k,196) &
                      + rxt(k,197)) * y(k,199) + (rxt(k,484) + rxt(k,490) + rxt(k,495) &
                      ) * y(k,110) + (rxt(k,489) + rxt(k,494) + rxt(k,499)) * y(k,81) &
                      + (rxt(k,491) + rxt(k,496)) * y(k,109))
         mat(k,1904) = -rxt(k,184)*y(k,103)
         mat(k,1559) = -rxt(k,185)*y(k,103)
         mat(k,1417) = -(rxt(k,196) + rxt(k,197)) * y(k,103)
         mat(k,665) = -(rxt(k,484) + rxt(k,490) + rxt(k,495)) * y(k,103)
         mat(k,784) = -(rxt(k,489) + rxt(k,494) + rxt(k,499)) * y(k,103)
         mat(k,610) = -(rxt(k,491) + rxt(k,496)) * y(k,103)
         mat(k,150) = rxt(k,276)*y(k,77)
         mat(k,1796) = rxt(k,164)*y(k,77)
         mat(k,1593) = rxt(k,276)*y(k,49) + rxt(k,164)*y(k,63) + rxt(k,166)*y(k,95) &
                      + rxt(k,167)*y(k,97) + rxt(k,186)*y(k,110) + rxt(k,168)*y(k,189)
         mat(k,1619) = rxt(k,183)*y(k,200)
         mat(k,965) = rxt(k,166)*y(k,77)
         mat(k,434) = rxt(k,167)*y(k,77)
         mat(k,665) = mat(k,665) + rxt(k,186)*y(k,77)
         mat(k,1343) = rxt(k,168)*y(k,77)
         mat(k,1559) = mat(k,1559) + rxt(k,183)*y(k,80)
         mat(k,74) = -(rxt(k,256)*y(k,200) + rxt(k,264)*y(k,199))
         mat(k,1443) = -rxt(k,256)*y(k,104)
         mat(k,1410) = -rxt(k,264)*y(k,104)
         mat(k,625) = -(rxt(k,257)*y(k,200))
         mat(k,1515) = -rxt(k,257)*y(k,105)
         mat(k,740) = .050_r8*rxt(k,430)*y(k,144)
         mat(k,172) = .350_r8*rxt(k,267)*y(k,200)
         mat(k,419) = .370_r8*rxt(k,269)*y(k,144)
         mat(k,802) = .120_r8*rxt(k,298)*y(k,144)
         mat(k,672) = .110_r8*rxt(k,375)*y(k,144)
         mat(k,1013) = .330_r8*rxt(k,328)*y(k,144)
         mat(k,719) = .050_r8*rxt(k,433)*y(k,144)
         mat(k,1079) = .120_r8*rxt(k,342)*y(k,144)
         mat(k,1841) = rxt(k,260)*y(k,190)
         mat(k,1644) = .050_r8*rxt(k,430)*y(k,27) + .370_r8*rxt(k,269)*y(k,46) &
                      + .120_r8*rxt(k,298)*y(k,50) + .110_r8*rxt(k,375)*y(k,116) &
                      + .330_r8*rxt(k,328)*y(k,122) + .050_r8*rxt(k,433)*y(k,127) &
                      + .120_r8*rxt(k,342)*y(k,128)
         mat(k,1309) = rxt(k,258)*y(k,190)
         mat(k,307) = rxt(k,260)*y(k,138) + rxt(k,258)*y(k,189)
         mat(k,1515) = mat(k,1515) + .350_r8*rxt(k,267)*y(k,45)
         mat(k,616) = rxt(k,222)*y(k,95) + rxt(k,224)*y(k,107) + rxt(k,223)*y(k,210)
         mat(k,960) = rxt(k,222)*y(k,91)
         mat(k,1215) = rxt(k,224)*y(k,91)
         mat(k,1944) = rxt(k,223)*y(k,91)
         mat(k,1218) = -(rxt(k,161)*y(k,200) + rxt(k,224)*y(k,91))
         mat(k,1557) = -rxt(k,161)*y(k,107)
         mat(k,619) = -rxt(k,224)*y(k,107)
         mat(k,1794) = rxt(k,245)*y(k,140)
         mat(k,953) = rxt(k,278)*y(k,140)
         mat(k,1029) = rxt(k,304)*y(k,140)
         mat(k,783) = (rxt(k,489)+rxt(k,494)+rxt(k,499))*y(k,103)
         mat(k,158) = rxt(k,466)*y(k,140)
         mat(k,1249) = (rxt(k,489)+rxt(k,494)+rxt(k,499))*y(k,81)
         mat(k,1715) = rxt(k,160)*y(k,200)
         mat(k,1771) = rxt(k,245)*y(k,63) + rxt(k,278)*y(k,66) + rxt(k,304)*y(k,70) &
                      + rxt(k,466)*y(k,88)
         mat(k,1557) = mat(k,1557) + rxt(k,160)*y(k,139)
         mat(k,238) = -(rxt(k,139)*y(k,200))
         mat(k,1468) = -rxt(k,139)*y(k,108)
         mat(k,1693) = rxt(k,158)*y(k,189)
         mat(k,1274) = rxt(k,158)*y(k,139)
         mat(k,608) = -(rxt(k,215)*y(k,143) + (rxt(k,491) + rxt(k,496)) * y(k,103))
         mat(k,1893) = -rxt(k,215)*y(k,109)
         mat(k,1245) = -(rxt(k,491) + rxt(k,496)) * y(k,109)
         mat(k,1922) = rxt(k,207)*y(k,189)
         mat(k,1308) = rxt(k,207)*y(k,40)
         mat(k,664) = -(rxt(k,186)*y(k,77) + rxt(k,187)*y(k,143) + rxt(k,188)*y(k,200) &
                      + (rxt(k,484) + rxt(k,490) + rxt(k,495)) * y(k,103))
         mat(k,1582) = -rxt(k,186)*y(k,110)
         mat(k,1895) = -rxt(k,187)*y(k,110)
         mat(k,1520) = -rxt(k,188)*y(k,110)
         mat(k,1246) = -(rxt(k,484) + rxt(k,490) + rxt(k,495)) * y(k,110)
         mat(k,1612) = rxt(k,175)*y(k,189)
         mat(k,781) = rxt(k,180)*y(k,200)
         mat(k,1313) = rxt(k,175)*y(k,80)
         mat(k,1520) = mat(k,1520) + rxt(k,180)*y(k,81)
         mat(k,853) = -(rxt(k,321)*y(k,200))
         mat(k,1536) = -rxt(k,321)*y(k,111)
         mat(k,455) = .300_r8*rxt(k,366)*y(k,200)
         mat(k,384) = .500_r8*rxt(k,367)*y(k,200)
         mat(k,1853) = rxt(k,320)*y(k,186) + rxt(k,327)*y(k,193)
         mat(k,427) = rxt(k,320)*y(k,138)
         mat(k,1102) = rxt(k,327)*y(k,138)
         mat(k,1536) = mat(k,1536) + .300_r8*rxt(k,366)*y(k,117) + .500_r8*rxt(k,367) &
                      *y(k,118)
         mat(k,120) = -(rxt(k,352)*y(k,200))
         mat(k,1451) = -rxt(k,352)*y(k,112)
         mat(k,870) = -(rxt(k,306)*y(k,200))
         mat(k,1538) = -rxt(k,306)*y(k,113)
         mat(k,456) = .700_r8*rxt(k,366)*y(k,200)
         mat(k,385) = .500_r8*rxt(k,367)*y(k,200)
         mat(k,390) = .500_r8*rxt(k,341)*y(k,200)
         mat(k,1855) = .050_r8*rxt(k,364)*y(k,192) + .220_r8*rxt(k,326)*y(k,193) &
                      + .250_r8*rxt(k,383)*y(k,207)
         mat(k,1753) = .050_r8*rxt(k,365)*y(k,192) + .220_r8*rxt(k,325)*y(k,193) &
                      + .250_r8*rxt(k,384)*y(k,207)
         mat(k,403) = .500_r8*rxt(k,310)*y(k,200)
         mat(k,1181) = .220_r8*rxt(k,322)*y(k,193) + .250_r8*rxt(k,380)*y(k,207)
         mat(k,1375) = .230_r8*rxt(k,323)*y(k,193) + .200_r8*rxt(k,311)*y(k,203) &
                      + .100_r8*rxt(k,381)*y(k,207)
         mat(k,1133) = .050_r8*rxt(k,364)*y(k,138) + .050_r8*rxt(k,365)*y(k,140)
         mat(k,1104) = .220_r8*rxt(k,326)*y(k,138) + .220_r8*rxt(k,325)*y(k,140) &
                      + .220_r8*rxt(k,322)*y(k,183) + .230_r8*rxt(k,323)*y(k,184)
         mat(k,1538) = mat(k,1538) + .700_r8*rxt(k,366)*y(k,117) + .500_r8*rxt(k,367) &
                      *y(k,118) + .500_r8*rxt(k,341)*y(k,126) + .500_r8*rxt(k,310) &
                      *y(k,153)
         mat(k,939) = .200_r8*rxt(k,311)*y(k,184)
         mat(k,978) = .250_r8*rxt(k,383)*y(k,138) + .250_r8*rxt(k,384)*y(k,140) &
                      + .250_r8*rxt(k,380)*y(k,183) + .100_r8*rxt(k,381)*y(k,184)
      end do
      end subroutine nlnmat03
      subroutine nlnmat04( avec_len, mat, y, rxt )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k,187) = -(rxt(k,353)*y(k,200))
         mat(k,1461) = -rxt(k,353)*y(k,114)
         mat(k,1816) = .870_r8*rxt(k,364)*y(k,192)
         mat(k,1735) = .950_r8*rxt(k,365)*y(k,192)
         mat(k,1173) = rxt(k,360)*y(k,192)
         mat(k,1359) = .750_r8*rxt(k,361)*y(k,192)
         mat(k,1122) = .870_r8*rxt(k,364)*y(k,138) + .950_r8*rxt(k,365)*y(k,140) &
                      + rxt(k,360)*y(k,183) + .750_r8*rxt(k,361)*y(k,184)
         mat(k,47) = -(rxt(k,354)*y(k,200))
         mat(k,1438) = -rxt(k,354)*y(k,115)
         mat(k,519) = .600_r8*rxt(k,377)*y(k,200)
         mat(k,1438) = mat(k,1438) + .600_r8*rxt(k,377)*y(k,121)
         mat(k,673) = -(rxt(k,368)*y(k,140) + rxt(k,375)*y(k,144) + rxt(k,376) &
                      *y(k,200))
         mat(k,1738) = -rxt(k,368)*y(k,116)
         mat(k,1645) = -rxt(k,375)*y(k,116)
         mat(k,1521) = -rxt(k,376)*y(k,116)
         mat(k,453) = -(rxt(k,366)*y(k,200))
         mat(k,1499) = -rxt(k,366)*y(k,117)
         mat(k,1830) = .080_r8*rxt(k,358)*y(k,191)
         mat(k,1051) = .080_r8*rxt(k,358)*y(k,138)
         mat(k,381) = -(rxt(k,367)*y(k,200))
         mat(k,1490) = -rxt(k,367)*y(k,118)
         mat(k,1827) = .080_r8*rxt(k,364)*y(k,192)
         mat(k,1123) = .080_r8*rxt(k,364)*y(k,138)
         mat(k,999) = -(rxt(k,369)*y(k,183) + rxt(k,370)*y(k,184) + rxt(k,371) &
                      *y(k,189) + rxt(k,372)*y(k,138) + rxt(k,373)*y(k,140))
         mat(k,1183) = -rxt(k,369)*y(k,119)
         mat(k,1382) = -rxt(k,370)*y(k,119)
         mat(k,1331) = -rxt(k,371)*y(k,119)
         mat(k,1862) = -rxt(k,372)*y(k,119)
         mat(k,1760) = -rxt(k,373)*y(k,119)
         mat(k,676) = rxt(k,368)*y(k,140)
         mat(k,1760) = mat(k,1760) + rxt(k,368)*y(k,116)
         mat(k,250) = -(rxt(k,374)*y(k,200))
         mat(k,1470) = -rxt(k,374)*y(k,120)
         mat(k,990) = rxt(k,371)*y(k,189)
         mat(k,1276) = rxt(k,371)*y(k,119)
         mat(k,520) = -(rxt(k,377)*y(k,200))
         mat(k,1506) = -rxt(k,377)*y(k,121)
         mat(k,1300) = rxt(k,357)*y(k,191) + rxt(k,362)*y(k,192)
         mat(k,1052) = rxt(k,357)*y(k,189)
         mat(k,1125) = rxt(k,362)*y(k,189)
         mat(k,1015) = -(rxt(k,328)*y(k,144) + rxt(k,329)*y(k,200))
         mat(k,1664) = -rxt(k,328)*y(k,122)
         mat(k,1547) = -rxt(k,329)*y(k,122)
         mat(k,677) = .300_r8*rxt(k,375)*y(k,144)
         mat(k,1863) = .360_r8*rxt(k,358)*y(k,191)
         mat(k,1761) = .400_r8*rxt(k,359)*y(k,191)
         mat(k,1664) = mat(k,1664) + .300_r8*rxt(k,375)*y(k,116)
         mat(k,1184) = .390_r8*rxt(k,355)*y(k,191)
         mat(k,1383) = .310_r8*rxt(k,356)*y(k,191)
         mat(k,1061) = .360_r8*rxt(k,358)*y(k,138) + .400_r8*rxt(k,359)*y(k,140) &
                      + .390_r8*rxt(k,355)*y(k,183) + .310_r8*rxt(k,356)*y(k,184)
         mat(k,190) = -(rxt(k,330)*y(k,200))
         mat(k,1462) = -rxt(k,330)*y(k,123)
         mat(k,1270) = rxt(k,324)*y(k,193)
         mat(k,1099) = rxt(k,324)*y(k,189)
         mat(k,363) = -(rxt(k,339)*y(k,200))
         mat(k,1487) = -rxt(k,339)*y(k,124)
         mat(k,1825) = .800_r8*rxt(k,348)*y(k,177)
         mat(k,759) = .800_r8*rxt(k,348)*y(k,138)
         mat(k,195) = -(rxt(k,340)*y(k,200))
         mat(k,1463) = -rxt(k,340)*y(k,125)
         mat(k,1271) = .800_r8*rxt(k,337)*y(k,197)
         mat(k,511) = .800_r8*rxt(k,337)*y(k,189)
         mat(k,389) = -(rxt(k,341)*y(k,200))
         mat(k,1491) = -rxt(k,341)*y(k,126)
         mat(k,1698) = rxt(k,344)*y(k,195)
         mat(k,1156) = rxt(k,344)*y(k,139)
         mat(k,721) = -(rxt(k,432)*y(k,140) + rxt(k,433)*y(k,144) + rxt(k,434) &
                      *y(k,200))
         mat(k,1742) = -rxt(k,432)*y(k,127)
         mat(k,1648) = -rxt(k,433)*y(k,127)
         mat(k,1526) = -rxt(k,434)*y(k,127)
         mat(k,1085) = -(rxt(k,342)*y(k,144) + rxt(k,343)*y(k,200))
         mat(k,1668) = -rxt(k,342)*y(k,128)
         mat(k,1551) = -rxt(k,343)*y(k,128)
         mat(k,679) = .200_r8*rxt(k,375)*y(k,144)
         mat(k,1866) = .560_r8*rxt(k,358)*y(k,191)
         mat(k,1765) = .600_r8*rxt(k,359)*y(k,191)
         mat(k,1668) = mat(k,1668) + .200_r8*rxt(k,375)*y(k,116)
         mat(k,1187) = .610_r8*rxt(k,355)*y(k,191)
         mat(k,1386) = .440_r8*rxt(k,356)*y(k,191)
         mat(k,1064) = .560_r8*rxt(k,358)*y(k,138) + .600_r8*rxt(k,359)*y(k,140) &
                      + .610_r8*rxt(k,355)*y(k,183) + .440_r8*rxt(k,356)*y(k,184)
         mat(k,256) = -(rxt(k,140)*y(k,138) + (rxt(k,141) + rxt(k,142) + rxt(k,143) &
                      ) * y(k,139) + rxt(k,152)*y(k,200))
         mat(k,1817) = -rxt(k,140)*y(k,129)
         mat(k,1694) = -(rxt(k,141) + rxt(k,142) + rxt(k,143)) * y(k,129)
         mat(k,1471) = -rxt(k,152)*y(k,129)
         mat(k,1692) = rxt(k,159)*y(k,140)
         mat(k,1734) = rxt(k,159)*y(k,139)
         mat(k,262) = -(rxt(k,378)*y(k,200))
         mat(k,1472) = -rxt(k,378)*y(k,132)
         mat(k,991) = .200_r8*rxt(k,370)*y(k,184)
         mat(k,1360) = .200_r8*rxt(k,370)*y(k,119)
         mat(k,829) = -(rxt(k,379)*y(k,200))
         mat(k,1534) = -rxt(k,379)*y(k,133)
         mat(k,996) = rxt(k,372)*y(k,138) + rxt(k,373)*y(k,140) + rxt(k,369)*y(k,183) &
                      + .800_r8*rxt(k,370)*y(k,184)
         mat(k,1851) = rxt(k,372)*y(k,119)
         mat(k,1749) = rxt(k,373)*y(k,119)
         mat(k,1179) = rxt(k,369)*y(k,119)
         mat(k,1372) = .800_r8*rxt(k,370)*y(k,119)
         mat(k,31) = -(rxt(k,468)*y(k,200))
         mat(k,1435) = -rxt(k,468)*y(k,134)
         mat(k,1885) = -(rxt(k,140)*y(k,129) + rxt(k,149)*y(k,140) + rxt(k,153) &
                      *y(k,189) + rxt(k,154)*y(k,144) + rxt(k,155)*y(k,143) + rxt(k,176) &
                      *y(k,80) + rxt(k,208)*y(k,40) + rxt(k,251)*y(k,184) + rxt(k,260) &
                      *y(k,190) + rxt(k,273)*y(k,180) + rxt(k,284)*y(k,183) + rxt(k,288) &
                      *y(k,188) + rxt(k,301)*y(k,181) + rxt(k,309)*y(k,202) + rxt(k,313) &
                      *y(k,203) + (rxt(k,319) + rxt(k,320)) * y(k,186) + (rxt(k,326) &
                      + rxt(k,327)) * y(k,193) + rxt(k,335)*y(k,195) + rxt(k,338) &
                      *y(k,197) + (rxt(k,348) + rxt(k,349)) * y(k,177) + rxt(k,358) &
                      *y(k,191) + rxt(k,364)*y(k,192) + rxt(k,372)*y(k,119) + rxt(k,383) &
                      *y(k,207) + rxt(k,387)*y(k,176) + rxt(k,390)*y(k,178) + rxt(k,395) &
                      *y(k,179) + rxt(k,397)*y(k,182) + rxt(k,401)*y(k,185) + rxt(k,404) &
                      *y(k,194) + rxt(k,407)*y(k,196) + rxt(k,410)*y(k,201) + rxt(k,417) &
                      *y(k,206) + rxt(k,423)*y(k,208) + rxt(k,426)*y(k,209) + rxt(k,437) &
                      *y(k,198) + rxt(k,442)*y(k,204) + rxt(k,447)*y(k,205))
         mat(k,260) = -rxt(k,140)*y(k,138)
         mat(k,1784) = -rxt(k,149)*y(k,138)
         mat(k,1354) = -rxt(k,153)*y(k,138)
         mat(k,1687) = -rxt(k,154)*y(k,138)
         mat(k,1915) = -rxt(k,155)*y(k,138)
         mat(k,1630) = -rxt(k,176)*y(k,138)
         mat(k,1939) = -rxt(k,208)*y(k,138)
         mat(k,1404) = -rxt(k,251)*y(k,138)
         mat(k,311) = -rxt(k,260)*y(k,138)
         mat(k,662) = -rxt(k,273)*y(k,138)
         mat(k,1201) = -rxt(k,284)*y(k,138)
         mat(k,550) = -rxt(k,288)*y(k,138)
         mat(k,638) = -rxt(k,301)*y(k,138)
         mat(k,605) = -rxt(k,309)*y(k,138)
         mat(k,948) = -rxt(k,313)*y(k,138)
         mat(k,432) = -(rxt(k,319) + rxt(k,320)) * y(k,138)
         mat(k,1119) = -(rxt(k,326) + rxt(k,327)) * y(k,138)
         mat(k,1171) = -rxt(k,335)*y(k,138)
         mat(k,518) = -rxt(k,338)*y(k,138)
         mat(k,773) = -(rxt(k,348) + rxt(k,349)) * y(k,138)
         mat(k,1077) = -rxt(k,358)*y(k,138)
         mat(k,1153) = -rxt(k,364)*y(k,138)
         mat(k,1012) = -rxt(k,372)*y(k,138)
         mat(k,989) = -rxt(k,383)*y(k,138)
         mat(k,380) = -rxt(k,387)*y(k,138)
         mat(k,348) = -rxt(k,390)*y(k,138)
         mat(k,305) = -rxt(k,395)*y(k,138)
         mat(k,475) = -rxt(k,397)*y(k,138)
         mat(k,596) = -rxt(k,401)*y(k,138)
         mat(k,556) = -rxt(k,404)*y(k,138)
         mat(k,714) = -rxt(k,407)*y(k,138)
         mat(k,318) = -rxt(k,410)*y(k,138)
         mat(k,571) = -rxt(k,417)*y(k,138)
         mat(k,588) = -rxt(k,423)*y(k,138)
         mat(k,356) = -rxt(k,426)*y(k,138)
         mat(k,935) = -rxt(k,437)*y(k,138)
         mat(k,915) = -rxt(k,442)*y(k,138)
         mat(k,896) = -rxt(k,447)*y(k,138)
         mat(k,260) = mat(k,260) + 2.000_r8*rxt(k,142)*y(k,139) + rxt(k,152)*y(k,200)
         mat(k,1728) = 2.000_r8*rxt(k,142)*y(k,129) + rxt(k,145)*y(k,143) + rxt(k,459) &
                      *y(k,157)
         mat(k,1915) = mat(k,1915) + rxt(k,145)*y(k,139)
         mat(k,1047) = rxt(k,459)*y(k,139)
         mat(k,1570) = rxt(k,152)*y(k,129)
         mat(k,1725) = -((rxt(k,141) + rxt(k,142) + rxt(k,143)) * y(k,129) + (rxt(k,145) &
                      + rxt(k,147)) * y(k,143) + rxt(k,146)*y(k,144) + rxt(k,158) &
                      *y(k,189) + rxt(k,159)*y(k,140) + rxt(k,160)*y(k,200) + rxt(k,178) &
                      *y(k,80) + rxt(k,209)*y(k,40) + rxt(k,295)*y(k,183) + rxt(k,344) &
                      *y(k,195) + rxt(k,402)*y(k,185) + rxt(k,405)*y(k,194) + rxt(k,408) &
                      *y(k,196) + rxt(k,412)*y(k,150) + rxt(k,415)*y(k,176) + rxt(k,459) &
                      *y(k,157))
         mat(k,259) = -(rxt(k,141) + rxt(k,142) + rxt(k,143)) * y(k,139)
         mat(k,1912) = -(rxt(k,145) + rxt(k,147)) * y(k,139)
         mat(k,1684) = -rxt(k,146)*y(k,139)
         mat(k,1351) = -rxt(k,158)*y(k,139)
         mat(k,1781) = -rxt(k,159)*y(k,139)
         mat(k,1567) = -rxt(k,160)*y(k,139)
         mat(k,1627) = -rxt(k,178)*y(k,139)
         mat(k,1936) = -rxt(k,209)*y(k,139)
         mat(k,1198) = -rxt(k,295)*y(k,139)
         mat(k,1168) = -rxt(k,344)*y(k,139)
         mat(k,595) = -rxt(k,402)*y(k,139)
         mat(k,555) = -rxt(k,405)*y(k,139)
         mat(k,713) = -rxt(k,408)*y(k,139)
         mat(k,332) = -rxt(k,412)*y(k,139)
         mat(k,379) = -rxt(k,415)*y(k,139)
         mat(k,1046) = -rxt(k,459)*y(k,139)
         mat(k,485) = rxt(k,346)*y(k,200)
         mat(k,229) = rxt(k,317)*y(k,140)
         mat(k,1936) = mat(k,1936) + rxt(k,208)*y(k,138)
         mat(k,1627) = mat(k,1627) + rxt(k,176)*y(k,138)
         mat(k,241) = rxt(k,139)*y(k,200)
         mat(k,460) = .700_r8*rxt(k,366)*y(k,200)
         mat(k,1009) = rxt(k,372)*y(k,138) + rxt(k,373)*y(k,140)
         mat(k,1882) = rxt(k,208)*y(k,40) + rxt(k,176)*y(k,80) + rxt(k,372)*y(k,119) &
                      + 2.000_r8*rxt(k,149)*y(k,140) + rxt(k,155)*y(k,143) &
                      + rxt(k,154)*y(k,144) + rxt(k,387)*y(k,176) + rxt(k,348) &
                      *y(k,177) + rxt(k,390)*y(k,178) + rxt(k,395)*y(k,179) &
                      + rxt(k,273)*y(k,180) + rxt(k,301)*y(k,181) + rxt(k,397) &
                      *y(k,182) + rxt(k,284)*y(k,183) + rxt(k,251)*y(k,184) &
                      + rxt(k,401)*y(k,185) + rxt(k,319)*y(k,186) + rxt(k,288) &
                      *y(k,188) + rxt(k,153)*y(k,189) + rxt(k,260)*y(k,190) &
                      + .920_r8*rxt(k,358)*y(k,191) + .920_r8*rxt(k,364)*y(k,192) &
                      + rxt(k,326)*y(k,193) + rxt(k,404)*y(k,194) + rxt(k,335) &
                      *y(k,195) + rxt(k,407)*y(k,196) + rxt(k,338)*y(k,197) &
                      + 1.600_r8*rxt(k,437)*y(k,198) + rxt(k,410)*y(k,201) &
                      + rxt(k,309)*y(k,202) + rxt(k,313)*y(k,203) + .900_r8*rxt(k,442) &
                      *y(k,204) + .800_r8*rxt(k,447)*y(k,205) + rxt(k,417)*y(k,206) &
                      + rxt(k,383)*y(k,207) + rxt(k,423)*y(k,208) + rxt(k,426) &
                      *y(k,209)
         mat(k,1781) = mat(k,1781) + rxt(k,317)*y(k,37) + rxt(k,373)*y(k,119) &
                      + 2.000_r8*rxt(k,149)*y(k,138) + rxt(k,150)*y(k,143) &
                      + rxt(k,148)*y(k,189) + rxt(k,359)*y(k,191) + rxt(k,365) &
                      *y(k,192) + rxt(k,325)*y(k,193) + rxt(k,336)*y(k,195) &
                      + 2.000_r8*rxt(k,438)*y(k,198) + rxt(k,151)*y(k,200) &
                      + rxt(k,384)*y(k,207)
         mat(k,700) = rxt(k,307)*y(k,200)
         mat(k,1912) = mat(k,1912) + rxt(k,155)*y(k,138) + rxt(k,150)*y(k,140)
         mat(k,1684) = mat(k,1684) + rxt(k,154)*y(k,138)
         mat(k,467) = rxt(k,444)*y(k,200)
         mat(k,379) = mat(k,379) + rxt(k,387)*y(k,138)
         mat(k,771) = rxt(k,348)*y(k,138)
         mat(k,347) = rxt(k,390)*y(k,138)
         mat(k,304) = rxt(k,395)*y(k,138)
         mat(k,660) = rxt(k,273)*y(k,138)
         mat(k,636) = rxt(k,301)*y(k,138)
         mat(k,474) = rxt(k,397)*y(k,138)
         mat(k,1198) = mat(k,1198) + rxt(k,284)*y(k,138)
         mat(k,1401) = rxt(k,251)*y(k,138) + .500_r8*rxt(k,435)*y(k,198)
         mat(k,595) = mat(k,595) + rxt(k,401)*y(k,138)
         mat(k,430) = rxt(k,319)*y(k,138)
         mat(k,548) = rxt(k,288)*y(k,138)
         mat(k,1351) = mat(k,1351) + rxt(k,153)*y(k,138) + rxt(k,148)*y(k,140)
         mat(k,309) = rxt(k,260)*y(k,138)
         mat(k,1074) = .920_r8*rxt(k,358)*y(k,138) + rxt(k,359)*y(k,140)
         mat(k,1150) = .920_r8*rxt(k,364)*y(k,138) + rxt(k,365)*y(k,140)
         mat(k,1116) = rxt(k,326)*y(k,138) + rxt(k,325)*y(k,140)
         mat(k,555) = mat(k,555) + rxt(k,404)*y(k,138)
         mat(k,1168) = mat(k,1168) + rxt(k,335)*y(k,138) + rxt(k,336)*y(k,140)
         mat(k,713) = mat(k,713) + rxt(k,407)*y(k,138)
         mat(k,517) = rxt(k,338)*y(k,138)
         mat(k,932) = 1.600_r8*rxt(k,437)*y(k,138) + 2.000_r8*rxt(k,438)*y(k,140) &
                      + .500_r8*rxt(k,435)*y(k,184)
         mat(k,1567) = mat(k,1567) + rxt(k,346)*y(k,24) + rxt(k,139)*y(k,108) &
                      + .700_r8*rxt(k,366)*y(k,117) + rxt(k,151)*y(k,140) + rxt(k,307) &
                      *y(k,141) + rxt(k,444)*y(k,163)
         mat(k,317) = rxt(k,410)*y(k,138)
         mat(k,603) = rxt(k,309)*y(k,138)
         mat(k,946) = rxt(k,313)*y(k,138)
         mat(k,913) = .900_r8*rxt(k,442)*y(k,138)
         mat(k,893) = .800_r8*rxt(k,447)*y(k,138)
         mat(k,570) = rxt(k,417)*y(k,138)
         mat(k,986) = rxt(k,383)*y(k,138) + rxt(k,384)*y(k,140)
         mat(k,587) = rxt(k,423)*y(k,138)
         mat(k,355) = rxt(k,426)*y(k,138)
      end do
      end subroutine nlnmat04
      subroutine nlnmat05( avec_len, mat, y, rxt )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k,1782) = -(rxt(k,148)*y(k,189) + rxt(k,149)*y(k,138) + rxt(k,150) &
                      *y(k,143) + rxt(k,151)*y(k,200) + rxt(k,159)*y(k,139) + rxt(k,245) &
                      *y(k,63) + rxt(k,278)*y(k,66) + rxt(k,297)*y(k,50) + rxt(k,304) &
                      *y(k,70) + rxt(k,317)*y(k,37) + rxt(k,325)*y(k,193) + rxt(k,336) &
                      *y(k,195) + rxt(k,359)*y(k,191) + rxt(k,365)*y(k,192) + rxt(k,368) &
                      *y(k,116) + rxt(k,373)*y(k,119) + rxt(k,384)*y(k,207) + rxt(k,429) &
                      *y(k,27) + rxt(k,432)*y(k,127) + rxt(k,438)*y(k,198) + rxt(k,449) &
                      *y(k,165) + rxt(k,466)*y(k,88))
         mat(k,1352) = -rxt(k,148)*y(k,140)
         mat(k,1883) = -rxt(k,149)*y(k,140)
         mat(k,1913) = -rxt(k,150)*y(k,140)
         mat(k,1568) = -rxt(k,151)*y(k,140)
         mat(k,1726) = -rxt(k,159)*y(k,140)
         mat(k,1805) = -rxt(k,245)*y(k,140)
         mat(k,958) = -rxt(k,278)*y(k,140)
         mat(k,815) = -rxt(k,297)*y(k,140)
         mat(k,1033) = -rxt(k,304)*y(k,140)
         mat(k,230) = -rxt(k,317)*y(k,140)
         mat(k,1117) = -rxt(k,325)*y(k,140)
         mat(k,1169) = -rxt(k,336)*y(k,140)
         mat(k,1075) = -rxt(k,359)*y(k,140)
         mat(k,1151) = -rxt(k,365)*y(k,140)
         mat(k,686) = -rxt(k,368)*y(k,140)
         mat(k,1010) = -rxt(k,373)*y(k,140)
         mat(k,987) = -rxt(k,384)*y(k,140)
         mat(k,756) = -rxt(k,429)*y(k,140)
         mat(k,735) = -rxt(k,432)*y(k,140)
         mat(k,933) = -rxt(k,438)*y(k,140)
         mat(k,799) = -rxt(k,449)*y(k,140)
         mat(k,161) = -rxt(k,466)*y(k,140)
         mat(k,414) = rxt(k,210)*y(k,143)
         mat(k,1602) = rxt(k,177)*y(k,81)
         mat(k,789) = rxt(k,177)*y(k,77) + rxt(k,179)*y(k,143) + rxt(k,180)*y(k,200)
         mat(k,623) = rxt(k,224)*y(k,107)
         mat(k,1226) = rxt(k,224)*y(k,91) + rxt(k,161)*y(k,200)
         mat(k,395) = .500_r8*rxt(k,341)*y(k,200)
         mat(k,1726) = mat(k,1726) + rxt(k,147)*y(k,143) + rxt(k,146)*y(k,144)
         mat(k,1913) = mat(k,1913) + rxt(k,210)*y(k,41) + rxt(k,179)*y(k,81) &
                      + rxt(k,147)*y(k,139)
         mat(k,1685) = rxt(k,146)*y(k,139)
         mat(k,327) = rxt(k,293)*y(k,200)
         mat(k,1568) = mat(k,1568) + rxt(k,180)*y(k,81) + rxt(k,161)*y(k,107) &
                      + .500_r8*rxt(k,341)*y(k,126) + rxt(k,293)*y(k,148)
         mat(k,696) = -(rxt(k,307)*y(k,200))
         mat(k,1523) = -rxt(k,307)*y(k,141)
         mat(k,803) = rxt(k,297)*y(k,140)
         mat(k,382) = .500_r8*rxt(k,367)*y(k,200)
         mat(k,252) = rxt(k,374)*y(k,200)
         mat(k,263) = rxt(k,378)*y(k,200)
         mat(k,826) = rxt(k,379)*y(k,200)
         mat(k,1740) = rxt(k,297)*y(k,50)
         mat(k,1523) = mat(k,1523) + .500_r8*rxt(k,367)*y(k,118) + rxt(k,374)*y(k,120) &
                      + rxt(k,378)*y(k,132) + rxt(k,379)*y(k,133)
         mat(k,274) = -(rxt(k,439)*y(k,200))
         mat(k,1474) = -rxt(k,439)*y(k,142)
         mat(k,1278) = rxt(k,436)*y(k,198)
         mat(k,917) = rxt(k,436)*y(k,189)
         mat(k,1916) = -(rxt(k,119)*y(k,144) + 4._r8*rxt(k,120)*y(k,143) + rxt(k,122) &
                      *y(k,95) + rxt(k,123)*y(k,97) + rxt(k,128)*y(k,189) + rxt(k,134) &
                      *y(k,200) + (rxt(k,145) + rxt(k,147)) * y(k,139) + rxt(k,150) &
                      *y(k,140) + rxt(k,155)*y(k,138) + rxt(k,179)*y(k,81) + rxt(k,181) &
                      *y(k,80) + rxt(k,184)*y(k,103) + rxt(k,187)*y(k,110) + rxt(k,210) &
                      *y(k,41) + rxt(k,211)*y(k,40) + rxt(k,213)*y(k,99) + rxt(k,215) &
                      *y(k,109) + rxt(k,246)*y(k,63) + rxt(k,452)*y(k,146))
         mat(k,1688) = -rxt(k,119)*y(k,143)
         mat(k,971) = -rxt(k,122)*y(k,143)
         mat(k,438) = -rxt(k,123)*y(k,143)
         mat(k,1355) = -rxt(k,128)*y(k,143)
         mat(k,1571) = -rxt(k,134)*y(k,143)
         mat(k,1729) = -(rxt(k,145) + rxt(k,147)) * y(k,143)
         mat(k,1785) = -rxt(k,150)*y(k,143)
         mat(k,1886) = -rxt(k,155)*y(k,143)
         mat(k,790) = -rxt(k,179)*y(k,143)
         mat(k,1631) = -rxt(k,181)*y(k,143)
         mat(k,1262) = -rxt(k,184)*y(k,143)
         mat(k,669) = -rxt(k,187)*y(k,143)
         mat(k,415) = -rxt(k,210)*y(k,143)
         mat(k,1940) = -rxt(k,211)*y(k,143)
         mat(k,649) = -rxt(k,213)*y(k,143)
         mat(k,613) = -rxt(k,215)*y(k,143)
         mat(k,1808) = -rxt(k,246)*y(k,143)
         mat(k,223) = -rxt(k,452)*y(k,143)
         mat(k,1241) = rxt(k,126)*y(k,189)
         mat(k,261) = rxt(k,140)*y(k,138) + rxt(k,141)*y(k,139)
         mat(k,1886) = mat(k,1886) + rxt(k,140)*y(k,129)
         mat(k,1729) = mat(k,1729) + rxt(k,141)*y(k,129)
         mat(k,1355) = mat(k,1355) + rxt(k,126)*y(k,94)
         mat(k,1571) = mat(k,1571) + 2.000_r8*rxt(k,136)*y(k,200)
         mat(k,1683) = -(rxt(k,118)*y(k,199) + rxt(k,119)*y(k,143) + rxt(k,129) &
                      *y(k,189) + rxt(k,130)*y(k,94) + rxt(k,135)*y(k,200) + rxt(k,146) &
                      *y(k,139) + rxt(k,154)*y(k,138) + rxt(k,170)*y(k,77) + rxt(k,202) &
                      *y(k,38) + rxt(k,269)*y(k,46) + rxt(k,298)*y(k,50) + rxt(k,328) &
                      *y(k,122) + rxt(k,342)*y(k,128) + rxt(k,375)*y(k,116) + rxt(k,413) &
                      *y(k,150) + rxt(k,430)*y(k,27) + rxt(k,433)*y(k,127) + rxt(k,455) &
                      *y(k,155) + rxt(k,461)*y(k,157))
         mat(k,1424) = -rxt(k,118)*y(k,144)
         mat(k,1911) = -rxt(k,119)*y(k,144)
         mat(k,1350) = -rxt(k,129)*y(k,144)
         mat(k,1238) = -rxt(k,130)*y(k,144)
         mat(k,1566) = -rxt(k,135)*y(k,144)
         mat(k,1724) = -rxt(k,146)*y(k,144)
         mat(k,1881) = -rxt(k,154)*y(k,144)
         mat(k,1600) = -rxt(k,170)*y(k,144)
         mat(k,1210) = -rxt(k,202)*y(k,144)
         mat(k,423) = -rxt(k,269)*y(k,144)
         mat(k,813) = -rxt(k,298)*y(k,144)
         mat(k,1023) = -rxt(k,328)*y(k,144)
         mat(k,1093) = -rxt(k,342)*y(k,144)
         mat(k,685) = -rxt(k,375)*y(k,144)
         mat(k,331) = -rxt(k,413)*y(k,144)
         mat(k,755) = -rxt(k,430)*y(k,144)
         mat(k,734) = -rxt(k,433)*y(k,144)
         mat(k,372) = -rxt(k,455)*y(k,144)
         mat(k,1045) = -rxt(k,461)*y(k,144)
         mat(k,1197) = .150_r8*rxt(k,283)*y(k,189)
         mat(k,1350) = mat(k,1350) + .150_r8*rxt(k,283)*y(k,183) + .150_r8*rxt(k,333) &
                      *y(k,195)
         mat(k,1167) = .150_r8*rxt(k,333)*y(k,189)
         mat(k,200) = -(rxt(k,462)*y(k,157))
         mat(k,1035) = -rxt(k,462)*y(k,145)
         mat(k,1920) = rxt(k,204)*y(k,80)
         mat(k,1611) = rxt(k,204)*y(k,40) + 2.000_r8*rxt(k,174)*y(k,80)
         mat(k,216) = -(rxt(k,452)*y(k,143) + rxt(k,453)*y(k,200))
         mat(k,1889) = -rxt(k,452)*y(k,146)
         mat(k,1466) = -rxt(k,453)*y(k,146)
         mat(k,850) = rxt(k,321)*y(k,200)
         mat(k,1812) = .100_r8*rxt(k,442)*y(k,204)
         mat(k,1453) = rxt(k,321)*y(k,111)
         mat(k,898) = .100_r8*rxt(k,442)*y(k,138)
         mat(k,322) = -(rxt(k,293)*y(k,200))
         mat(k,1481) = -rxt(k,293)*y(k,148)
         mat(k,1695) = rxt(k,295)*y(k,183)
         mat(k,1174) = rxt(k,295)*y(k,139)
         mat(k,1691) = rxt(k,415)*y(k,176)
         mat(k,374) = rxt(k,415)*y(k,139)
         mat(k,329) = -(rxt(k,412)*y(k,139) + rxt(k,413)*y(k,144))
         mat(k,1696) = -rxt(k,412)*y(k,150)
         mat(k,1638) = -rxt(k,413)*y(k,150)
         mat(k,87) = .070_r8*rxt(k,399)*y(k,200)
         mat(k,1822) = rxt(k,397)*y(k,182)
         mat(k,67) = .060_r8*rxt(k,411)*y(k,200)
         mat(k,107) = .070_r8*rxt(k,427)*y(k,200)
         mat(k,469) = rxt(k,397)*y(k,138)
         mat(k,1482) = .070_r8*rxt(k,399)*y(k,87) + .060_r8*rxt(k,411)*y(k,151) &
                      + .070_r8*rxt(k,427)*y(k,172)
         mat(k,65) = -(rxt(k,411)*y(k,200))
         mat(k,1441) = -rxt(k,411)*y(k,151)
         mat(k,57) = .530_r8*rxt(k,388)*y(k,200)
         mat(k,1441) = mat(k,1441) + .530_r8*rxt(k,388)*y(k,28)
         mat(k,205) = -(rxt(k,414)*y(k,200))
         mat(k,1464) = -rxt(k,414)*y(k,152)
         mat(k,1272) = rxt(k,409)*y(k,201)
         mat(k,312) = rxt(k,409)*y(k,189)
         mat(k,401) = -(rxt(k,310)*y(k,200))
         mat(k,1493) = -rxt(k,310)*y(k,153)
         mat(k,1293) = rxt(k,308)*y(k,202)
         mat(k,597) = rxt(k,308)*y(k,189)
         mat(k,280) = -(rxt(k,314)*y(k,200))
         mat(k,1475) = -rxt(k,314)*y(k,154)
         mat(k,1279) = .850_r8*rxt(k,312)*y(k,203)
         mat(k,937) = .850_r8*rxt(k,312)*y(k,189)
         mat(k,368) = -(rxt(k,455)*y(k,144) + rxt(k,458)*y(k,200))
         mat(k,1639) = -rxt(k,455)*y(k,155)
         mat(k,1488) = -rxt(k,458)*y(k,155)
         mat(k,1038) = -(rxt(k,456)*y(k,40) + rxt(k,457)*y(k,80) + rxt(k,459)*y(k,139) &
                      + rxt(k,461)*y(k,144) + rxt(k,462)*y(k,145) + rxt(k,463) &
                      *y(k,200))
         mat(k,1924) = -rxt(k,456)*y(k,157)
         mat(k,1615) = -rxt(k,457)*y(k,157)
         mat(k,1711) = -rxt(k,459)*y(k,157)
         mat(k,1666) = -rxt(k,461)*y(k,157)
         mat(k,202) = -rxt(k,462)*y(k,157)
         mat(k,1549) = -rxt(k,463)*y(k,157)
         mat(k,1900) = rxt(k,452)*y(k,146)
         mat(k,1666) = mat(k,1666) + rxt(k,455)*y(k,155)
         mat(k,220) = rxt(k,452)*y(k,143)
         mat(k,369) = rxt(k,455)*y(k,144) + rxt(k,458)*y(k,200)
         mat(k,1549) = mat(k,1549) + rxt(k,458)*y(k,155)
         mat(k,690) = -(rxt(k,464)*y(k,200))
         mat(k,1522) = -rxt(k,464)*y(k,158)
         mat(k,1923) = rxt(k,456)*y(k,157)
         mat(k,1613) = rxt(k,457)*y(k,157)
         mat(k,157) = rxt(k,466)*y(k,140) + (rxt(k,467)+.500_r8*rxt(k,469))*y(k,200)
         mat(k,1704) = rxt(k,459)*y(k,157)
         mat(k,1739) = rxt(k,466)*y(k,88)
         mat(k,1646) = rxt(k,461)*y(k,157)
         mat(k,201) = rxt(k,462)*y(k,157)
         mat(k,218) = rxt(k,453)*y(k,200)
         mat(k,1037) = rxt(k,456)*y(k,40) + rxt(k,457)*y(k,80) + rxt(k,459)*y(k,139) &
                      + rxt(k,461)*y(k,144) + rxt(k,462)*y(k,145) + rxt(k,463) &
                      *y(k,200)
         mat(k,1522) = mat(k,1522) + (rxt(k,467)+.500_r8*rxt(k,469))*y(k,88) &
                      + rxt(k,453)*y(k,146) + rxt(k,463)*y(k,157)
         mat(k,136) = -(rxt(k,465)*y(k,210))
         mat(k,1945) = -rxt(k,465)*y(k,159)
         mat(k,689) = rxt(k,464)*y(k,200)
         mat(k,1455) = rxt(k,464)*y(k,158)
         mat(k,80) = .100_r8*rxt(k,419)*y(k,200)
         mat(k,97) = .230_r8*rxt(k,421)*y(k,200)
         mat(k,1446) = .100_r8*rxt(k,419)*y(k,168) + .230_r8*rxt(k,421)*y(k,170)
         mat(k,444) = -(rxt(k,443)*y(k,200))
         mat(k,1498) = -rxt(k,443)*y(k,162)
         mat(k,1295) = rxt(k,441)*y(k,204)
         mat(k,899) = rxt(k,441)*y(k,189)
      end do
      end subroutine nlnmat05
      subroutine nlnmat06( avec_len, mat, y, rxt )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k,462) = -(rxt(k,444)*y(k,200))
         mat(k,1500) = -rxt(k,444)*y(k,163)
         mat(k,1831) = .200_r8*rxt(k,437)*y(k,198) + .200_r8*rxt(k,447)*y(k,205)
         mat(k,1363) = .500_r8*rxt(k,435)*y(k,198)
         mat(k,918) = .200_r8*rxt(k,437)*y(k,138) + .500_r8*rxt(k,435)*y(k,184)
         mat(k,877) = .200_r8*rxt(k,447)*y(k,138)
         mat(k,333) = -(rxt(k,448)*y(k,200))
         mat(k,1483) = -rxt(k,448)*y(k,164)
         mat(k,1287) = rxt(k,446)*y(k,205)
         mat(k,876) = rxt(k,446)*y(k,189)
         mat(k,792) = -(rxt(k,449)*y(k,140) + rxt(k,450)*y(k,200))
         mat(k,1746) = -rxt(k,449)*y(k,165)
         mat(k,1531) = -rxt(k,450)*y(k,165)
         mat(k,745) = .330_r8*rxt(k,430)*y(k,144)
         mat(k,724) = .330_r8*rxt(k,433)*y(k,144)
         mat(k,1849) = .800_r8*rxt(k,437)*y(k,198) + .800_r8*rxt(k,447)*y(k,205)
         mat(k,1746) = mat(k,1746) + rxt(k,438)*y(k,198)
         mat(k,1652) = .330_r8*rxt(k,430)*y(k,27) + .330_r8*rxt(k,433)*y(k,127)
         mat(k,463) = rxt(k,444)*y(k,200)
         mat(k,1370) = .500_r8*rxt(k,435)*y(k,198) + rxt(k,445)*y(k,205)
         mat(k,920) = .800_r8*rxt(k,437)*y(k,138) + rxt(k,438)*y(k,140) &
                      + .500_r8*rxt(k,435)*y(k,184)
         mat(k,1531) = mat(k,1531) + rxt(k,444)*y(k,163)
         mat(k,880) = .800_r8*rxt(k,447)*y(k,138) + rxt(k,445)*y(k,184)
         mat(k,841) = -(rxt(k,451)*y(k,200))
         mat(k,1535) = -rxt(k,451)*y(k,166)
         mat(k,746) = .300_r8*rxt(k,430)*y(k,144)
         mat(k,725) = .300_r8*rxt(k,433)*y(k,144)
         mat(k,1852) = .900_r8*rxt(k,442)*y(k,204)
         mat(k,1655) = .300_r8*rxt(k,430)*y(k,27) + .300_r8*rxt(k,433)*y(k,127)
         mat(k,1373) = rxt(k,440)*y(k,204)
         mat(k,903) = .900_r8*rxt(k,442)*y(k,138) + rxt(k,440)*y(k,184)
         mat(k,490) = -(rxt(k,418)*y(k,200))
         mat(k,1503) = -rxt(k,418)*y(k,167)
         mat(k,1297) = rxt(k,416)*y(k,206)
         mat(k,560) = rxt(k,416)*y(k,189)
         mat(k,78) = -(rxt(k,419)*y(k,200))
         mat(k,1444) = -rxt(k,419)*y(k,168)
         mat(k,94) = -(rxt(k,385)*y(k,200))
         mat(k,1447) = -rxt(k,385)*y(k,169)
         mat(k,1266) = rxt(k,382)*y(k,207)
         mat(k,973) = rxt(k,382)*y(k,189)
         mat(k,98) = -(rxt(k,421)*y(k,200))
         mat(k,1448) = -rxt(k,421)*y(k,170)
         mat(k,531) = -(rxt(k,424)*y(k,200))
         mat(k,1507) = -rxt(k,424)*y(k,171)
         mat(k,1301) = rxt(k,422)*y(k,208)
         mat(k,576) = rxt(k,422)*y(k,189)
         mat(k,106) = -(rxt(k,427)*y(k,200))
         mat(k,1449) = -rxt(k,427)*y(k,172)
         mat(k,99) = .150_r8*rxt(k,421)*y(k,200)
         mat(k,1449) = mat(k,1449) + .150_r8*rxt(k,421)*y(k,170)
         mat(k,292) = -(rxt(k,428)*y(k,200))
         mat(k,1477) = -rxt(k,428)*y(k,173)
         mat(k,1281) = rxt(k,425)*y(k,209)
         mat(k,349) = rxt(k,425)*y(k,189)
         mat(k,375) = -(rxt(k,386)*y(k,189) + rxt(k,387)*y(k,138) + rxt(k,415) &
                      *y(k,139))
         mat(k,1291) = -rxt(k,386)*y(k,176)
         mat(k,1826) = -rxt(k,387)*y(k,176)
         mat(k,1697) = -rxt(k,415)*y(k,176)
         mat(k,115) = rxt(k,392)*y(k,200)
         mat(k,1489) = rxt(k,392)*y(k,43)
         mat(k,764) = -(rxt(k,347)*y(k,189) + (rxt(k,348) + rxt(k,349)) * y(k,138))
         mat(k,1317) = -rxt(k,347)*y(k,177)
         mat(k,1847) = -(rxt(k,348) + rxt(k,349)) * y(k,177)
         mat(k,504) = rxt(k,350)*y(k,200)
         mat(k,126) = rxt(k,351)*y(k,200)
         mat(k,1528) = rxt(k,350)*y(k,25) + rxt(k,351)*y(k,36)
         mat(k,342) = -(rxt(k,389)*y(k,189) + rxt(k,390)*y(k,138))
         mat(k,1288) = -rxt(k,389)*y(k,178)
         mat(k,1823) = -rxt(k,390)*y(k,178)
         mat(k,58) = .350_r8*rxt(k,388)*y(k,200)
         mat(k,270) = rxt(k,391)*y(k,200)
         mat(k,1484) = .350_r8*rxt(k,388)*y(k,28) + rxt(k,391)*y(k,29)
         mat(k,300) = -(rxt(k,393)*y(k,189) + rxt(k,395)*y(k,138))
         mat(k,1282) = -rxt(k,393)*y(k,179)
         mat(k,1818) = -rxt(k,395)*y(k,179)
         mat(k,212) = rxt(k,394)*y(k,200)
         mat(k,81) = .070_r8*rxt(k,419)*y(k,200)
         mat(k,100) = .060_r8*rxt(k,421)*y(k,200)
         mat(k,1478) = rxt(k,394)*y(k,44) + .070_r8*rxt(k,419)*y(k,168) &
                      + .060_r8*rxt(k,421)*y(k,170)
         mat(k,655) = -(4._r8*rxt(k,270)*y(k,180) + rxt(k,271)*y(k,184) + rxt(k,272) &
                      *y(k,189) + rxt(k,273)*y(k,138))
         mat(k,1366) = -rxt(k,271)*y(k,180)
         mat(k,1312) = -rxt(k,272)*y(k,180)
         mat(k,1843) = -rxt(k,273)*y(k,180)
         mat(k,183) = .500_r8*rxt(k,275)*y(k,200)
         mat(k,149) = rxt(k,276)*y(k,77) + rxt(k,277)*y(k,200)
         mat(k,1581) = rxt(k,276)*y(k,49)
         mat(k,1519) = .500_r8*rxt(k,275)*y(k,48) + rxt(k,277)*y(k,49)
         mat(k,630) = -(rxt(k,299)*y(k,184) + rxt(k,300)*y(k,189) + rxt(k,301) &
                      *y(k,138))
         mat(k,1364) = -rxt(k,299)*y(k,181)
         mat(k,1310) = -rxt(k,300)*y(k,181)
         mat(k,1842) = -rxt(k,301)*y(k,181)
         mat(k,245) = rxt(k,302)*y(k,200)
         mat(k,35) = rxt(k,303)*y(k,200)
         mat(k,1516) = rxt(k,302)*y(k,51) + rxt(k,303)*y(k,52)
         mat(k,470) = -(rxt(k,396)*y(k,189) + rxt(k,397)*y(k,138))
         mat(k,1296) = -rxt(k,396)*y(k,182)
         mat(k,1832) = -rxt(k,397)*y(k,182)
         mat(k,133) = rxt(k,398)*y(k,200)
         mat(k,1832) = mat(k,1832) + rxt(k,387)*y(k,176)
         mat(k,1642) = rxt(k,413)*y(k,150)
         mat(k,330) = rxt(k,413)*y(k,144)
         mat(k,376) = rxt(k,387)*y(k,138) + .400_r8*rxt(k,386)*y(k,189)
         mat(k,1296) = mat(k,1296) + .400_r8*rxt(k,386)*y(k,176)
         mat(k,1501) = rxt(k,398)*y(k,53)
         mat(k,1191) = -(4._r8*rxt(k,281)*y(k,183) + rxt(k,282)*y(k,184) + rxt(k,283) &
                      *y(k,189) + rxt(k,284)*y(k,138) + rxt(k,295)*y(k,139) + rxt(k,322) &
                      *y(k,193) + rxt(k,355)*y(k,191) + rxt(k,360)*y(k,192) + rxt(k,369) &
                      *y(k,119) + rxt(k,380)*y(k,207))
         mat(k,1390) = -rxt(k,282)*y(k,183)
         mat(k,1339) = -rxt(k,283)*y(k,183)
         mat(k,1870) = -rxt(k,284)*y(k,183)
         mat(k,1713) = -rxt(k,295)*y(k,183)
         mat(k,1110) = -rxt(k,322)*y(k,183)
         mat(k,1067) = -rxt(k,355)*y(k,183)
         mat(k,1143) = -rxt(k,360)*y(k,183)
         mat(k,1003) = -rxt(k,369)*y(k,183)
         mat(k,981) = -rxt(k,380)*y(k,183)
         mat(k,752) = .060_r8*rxt(k,430)*y(k,144)
         mat(k,952) = rxt(k,278)*y(k,140) + rxt(k,279)*y(k,200)
         mat(k,1028) = rxt(k,304)*y(k,140) + rxt(k,305)*y(k,200)
         mat(k,358) = .500_r8*rxt(k,286)*y(k,200)
         mat(k,681) = .080_r8*rxt(k,375)*y(k,144)
         mat(k,1019) = .100_r8*rxt(k,328)*y(k,144)
         mat(k,731) = .060_r8*rxt(k,433)*y(k,144)
         mat(k,1087) = .280_r8*rxt(k,342)*y(k,144)
         mat(k,1870) = mat(k,1870) + .530_r8*rxt(k,326)*y(k,193) + rxt(k,335)*y(k,195) &
                      + rxt(k,338)*y(k,197) + rxt(k,313)*y(k,203)
         mat(k,1769) = rxt(k,278)*y(k,66) + rxt(k,304)*y(k,70) + .530_r8*rxt(k,325) &
                      *y(k,193) + rxt(k,336)*y(k,195)
         mat(k,1672) = .060_r8*rxt(k,430)*y(k,27) + .080_r8*rxt(k,375)*y(k,116) &
                      + .100_r8*rxt(k,328)*y(k,122) + .060_r8*rxt(k,433)*y(k,127) &
                      + .280_r8*rxt(k,342)*y(k,128)
         mat(k,844) = .650_r8*rxt(k,451)*y(k,200)
         mat(k,1191) = mat(k,1191) + .530_r8*rxt(k,322)*y(k,193)
         mat(k,1390) = mat(k,1390) + .260_r8*rxt(k,323)*y(k,193) + rxt(k,332)*y(k,195) &
                      + .300_r8*rxt(k,311)*y(k,203)
         mat(k,1339) = mat(k,1339) + .450_r8*rxt(k,333)*y(k,195) + .200_r8*rxt(k,337) &
                      *y(k,197) + .150_r8*rxt(k,312)*y(k,203)
         mat(k,1110) = mat(k,1110) + .530_r8*rxt(k,326)*y(k,138) + .530_r8*rxt(k,325) &
                      *y(k,140) + .530_r8*rxt(k,322)*y(k,183) + .260_r8*rxt(k,323) &
                      *y(k,184)
         mat(k,1161) = rxt(k,335)*y(k,138) + rxt(k,336)*y(k,140) + rxt(k,332)*y(k,184) &
                      + .450_r8*rxt(k,333)*y(k,189) + 4.000_r8*rxt(k,334)*y(k,195)
         mat(k,514) = rxt(k,338)*y(k,138) + .200_r8*rxt(k,337)*y(k,189)
         mat(k,1555) = rxt(k,279)*y(k,66) + rxt(k,305)*y(k,70) + .500_r8*rxt(k,286) &
                      *y(k,72) + .650_r8*rxt(k,451)*y(k,166)
         mat(k,942) = rxt(k,313)*y(k,138) + .300_r8*rxt(k,311)*y(k,184) &
                      + .150_r8*rxt(k,312)*y(k,189)
         mat(k,1395) = -(rxt(k,171)*y(k,80) + (4._r8*rxt(k,248) + 4._r8*rxt(k,249) &
                      ) * y(k,184) + rxt(k,250)*y(k,189) + rxt(k,251)*y(k,138) &
                      + rxt(k,271)*y(k,180) + rxt(k,282)*y(k,183) + rxt(k,299) &
                      *y(k,181) + rxt(k,311)*y(k,203) + rxt(k,323)*y(k,193) + rxt(k,332) &
                      *y(k,195) + rxt(k,356)*y(k,191) + rxt(k,361)*y(k,192) + rxt(k,370) &
                      *y(k,119) + rxt(k,381)*y(k,207) + rxt(k,435)*y(k,198) + rxt(k,440) &
                      *y(k,204) + rxt(k,445)*y(k,205))
         mat(k,1621) = -rxt(k,171)*y(k,184)
         mat(k,1345) = -rxt(k,250)*y(k,184)
         mat(k,1876) = -rxt(k,251)*y(k,184)
         mat(k,658) = -rxt(k,271)*y(k,184)
         mat(k,1195) = -rxt(k,282)*y(k,184)
         mat(k,634) = -rxt(k,299)*y(k,184)
         mat(k,944) = -rxt(k,311)*y(k,184)
         mat(k,1114) = -rxt(k,323)*y(k,184)
         mat(k,1165) = -rxt(k,332)*y(k,184)
         mat(k,1071) = -rxt(k,356)*y(k,184)
         mat(k,1147) = -rxt(k,361)*y(k,184)
         mat(k,1007) = -rxt(k,370)*y(k,184)
         mat(k,984) = -rxt(k,381)*y(k,184)
         mat(k,930) = -rxt(k,435)*y(k,184)
         mat(k,911) = -rxt(k,440)*y(k,184)
         mat(k,891) = -rxt(k,445)*y(k,184)
         mat(k,811) = .280_r8*rxt(k,298)*y(k,144)
         mat(k,398) = rxt(k,285)*y(k,200)
         mat(k,288) = .700_r8*rxt(k,253)*y(k,200)
         mat(k,683) = .050_r8*rxt(k,375)*y(k,144)
         mat(k,1007) = mat(k,1007) + rxt(k,369)*y(k,183)
         mat(k,1876) = mat(k,1876) + rxt(k,284)*y(k,183) + .830_r8*rxt(k,401)*y(k,185) &
                      + .170_r8*rxt(k,407)*y(k,196)
         mat(k,1678) = .280_r8*rxt(k,298)*y(k,50) + .050_r8*rxt(k,375)*y(k,116)
         mat(k,1195) = mat(k,1195) + rxt(k,369)*y(k,119) + rxt(k,284)*y(k,138) &
                      + 4.000_r8*rxt(k,281)*y(k,183) + .900_r8*rxt(k,282)*y(k,184) &
                      + .450_r8*rxt(k,283)*y(k,189) + rxt(k,355)*y(k,191) + rxt(k,360) &
                      *y(k,192) + rxt(k,322)*y(k,193) + rxt(k,331)*y(k,195) &
                      + rxt(k,380)*y(k,207)
         mat(k,1395) = mat(k,1395) + .900_r8*rxt(k,282)*y(k,183)
         mat(k,593) = .830_r8*rxt(k,401)*y(k,138) + .330_r8*rxt(k,400)*y(k,189)
         mat(k,1345) = mat(k,1345) + .450_r8*rxt(k,283)*y(k,183) + .330_r8*rxt(k,400) &
                      *y(k,185) + .070_r8*rxt(k,406)*y(k,196)
         mat(k,1071) = mat(k,1071) + rxt(k,355)*y(k,183)
         mat(k,1147) = mat(k,1147) + rxt(k,360)*y(k,183)
         mat(k,1114) = mat(k,1114) + rxt(k,322)*y(k,183)
         mat(k,1165) = mat(k,1165) + rxt(k,331)*y(k,183)
         mat(k,711) = .170_r8*rxt(k,407)*y(k,138) + .070_r8*rxt(k,406)*y(k,189)
         mat(k,1561) = rxt(k,285)*y(k,71) + .700_r8*rxt(k,253)*y(k,74)
         mat(k,984) = mat(k,984) + rxt(k,380)*y(k,183)
      end do
      end subroutine nlnmat06
      subroutine nlnmat07( avec_len, mat, y, rxt )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k,589) = -(rxt(k,400)*y(k,189) + rxt(k,401)*y(k,138) + rxt(k,402) &
                      *y(k,139))
         mat(k,1306) = -rxt(k,400)*y(k,185)
         mat(k,1839) = -rxt(k,401)*y(k,185)
         mat(k,1702) = -rxt(k,402)*y(k,185)
         mat(k,425) = -((rxt(k,319) + rxt(k,320)) * y(k,138))
         mat(k,1828) = -(rxt(k,319) + rxt(k,320)) * y(k,186)
         mat(k,225) = rxt(k,318)*y(k,200)
         mat(k,1495) = rxt(k,318)*y(k,37)
         mat(k,1813) = .750_r8*rxt(k,288)*y(k,188)
         mat(k,543) = .750_r8*rxt(k,288)*y(k,138)
         mat(k,544) = -(rxt(k,287)*y(k,189) + rxt(k,288)*y(k,138))
         mat(k,1302) = -rxt(k,287)*y(k,188)
         mat(k,1835) = -rxt(k,288)*y(k,188)
         mat(k,418) = rxt(k,294)*y(k,200)
         mat(k,1508) = rxt(k,294)*y(k,46)
         mat(k,1344) = -((rxt(k,124) + rxt(k,125) + rxt(k,126)) * y(k,94) + rxt(k,128) &
                      *y(k,143) + rxt(k,129)*y(k,144) + rxt(k,133)*y(k,200) &
                      + 4._r8*rxt(k,138)*y(k,189) + rxt(k,148)*y(k,140) + rxt(k,153) &
                      *y(k,138) + rxt(k,158)*y(k,139) + (rxt(k,168) + rxt(k,169) &
                      ) * y(k,77) + rxt(k,175)*y(k,80) + rxt(k,201)*y(k,38) + rxt(k,207) &
                      *y(k,40) + rxt(k,244)*y(k,63) + rxt(k,250)*y(k,184) + rxt(k,258) &
                      *y(k,190) + rxt(k,272)*y(k,180) + rxt(k,283)*y(k,183) + rxt(k,287) &
                      *y(k,188) + rxt(k,300)*y(k,181) + rxt(k,308)*y(k,202) + rxt(k,312) &
                      *y(k,203) + rxt(k,324)*y(k,193) + rxt(k,333)*y(k,195) + rxt(k,337) &
                      *y(k,197) + rxt(k,347)*y(k,177) + rxt(k,357)*y(k,191) + rxt(k,362) &
                      *y(k,192) + rxt(k,371)*y(k,119) + rxt(k,382)*y(k,207) + rxt(k,386) &
                      *y(k,176) + rxt(k,389)*y(k,178) + rxt(k,393)*y(k,179) + rxt(k,396) &
                      *y(k,182) + rxt(k,400)*y(k,185) + rxt(k,403)*y(k,194) + rxt(k,406) &
                      *y(k,196) + rxt(k,409)*y(k,201) + rxt(k,416)*y(k,206) + rxt(k,422) &
                      *y(k,208) + rxt(k,425)*y(k,209) + rxt(k,436)*y(k,198) + rxt(k,441) &
                      *y(k,204) + rxt(k,446)*y(k,205))
         mat(k,1233) = -(rxt(k,124) + rxt(k,125) + rxt(k,126)) * y(k,189)
         mat(k,1905) = -rxt(k,128)*y(k,189)
         mat(k,1677) = -rxt(k,129)*y(k,189)
         mat(k,1560) = -rxt(k,133)*y(k,189)
         mat(k,1774) = -rxt(k,148)*y(k,189)
         mat(k,1875) = -rxt(k,153)*y(k,189)
         mat(k,1718) = -rxt(k,158)*y(k,189)
         mat(k,1594) = -(rxt(k,168) + rxt(k,169)) * y(k,189)
         mat(k,1620) = -rxt(k,175)*y(k,189)
         mat(k,1207) = -rxt(k,201)*y(k,189)
         mat(k,1929) = -rxt(k,207)*y(k,189)
         mat(k,1797) = -rxt(k,244)*y(k,189)
         mat(k,1394) = -rxt(k,250)*y(k,189)
         mat(k,308) = -rxt(k,258)*y(k,189)
         mat(k,657) = -rxt(k,272)*y(k,189)
         mat(k,1194) = -rxt(k,283)*y(k,189)
         mat(k,546) = -rxt(k,287)*y(k,189)
         mat(k,633) = -rxt(k,300)*y(k,189)
         mat(k,601) = -rxt(k,308)*y(k,189)
         mat(k,943) = -rxt(k,312)*y(k,189)
         mat(k,1113) = -rxt(k,324)*y(k,189)
         mat(k,1164) = -rxt(k,333)*y(k,189)
         mat(k,515) = -rxt(k,337)*y(k,189)
         mat(k,768) = -rxt(k,347)*y(k,189)
         mat(k,1070) = -rxt(k,357)*y(k,189)
         mat(k,1146) = -rxt(k,362)*y(k,189)
         mat(k,1006) = -rxt(k,371)*y(k,189)
         mat(k,983) = -rxt(k,382)*y(k,189)
         mat(k,377) = -rxt(k,386)*y(k,189)
         mat(k,345) = -rxt(k,389)*y(k,189)
         mat(k,302) = -rxt(k,393)*y(k,189)
         mat(k,471) = -rxt(k,396)*y(k,189)
         mat(k,592) = -rxt(k,400)*y(k,189)
         mat(k,554) = -rxt(k,403)*y(k,189)
         mat(k,710) = -rxt(k,406)*y(k,189)
         mat(k,315) = -rxt(k,409)*y(k,189)
         mat(k,568) = -rxt(k,416)*y(k,189)
         mat(k,585) = -rxt(k,422)*y(k,189)
         mat(k,353) = -rxt(k,425)*y(k,189)
         mat(k,929) = -rxt(k,436)*y(k,189)
         mat(k,910) = -rxt(k,441)*y(k,189)
         mat(k,890) = -rxt(k,446)*y(k,189)
         mat(k,753) = .570_r8*rxt(k,430)*y(k,144)
         mat(k,59) = .650_r8*rxt(k,388)*y(k,200)
         mat(k,1207) = mat(k,1207) + rxt(k,200)*y(k,63)
         mat(k,1929) = mat(k,1929) + rxt(k,212)*y(k,200)
         mat(k,175) = .350_r8*rxt(k,267)*y(k,200)
         mat(k,421) = .130_r8*rxt(k,269)*y(k,144)
         mat(k,142) = rxt(k,274)*y(k,200)
         mat(k,810) = .280_r8*rxt(k,298)*y(k,144)
         mat(k,1797) = mat(k,1797) + rxt(k,200)*y(k,38) + rxt(k,164)*y(k,77) &
                      + rxt(k,245)*y(k,140) + rxt(k,246)*y(k,143)
         mat(k,26) = rxt(k,280)*y(k,200)
         mat(k,641) = rxt(k,252)*y(k,200)
         mat(k,1594) = mat(k,1594) + rxt(k,164)*y(k,63) + rxt(k,167)*y(k,97)
         mat(k,1620) = mat(k,1620) + rxt(k,171)*y(k,184) + rxt(k,182)*y(k,200)
         mat(k,868) = rxt(k,255)*y(k,200)
         mat(k,88) = .730_r8*rxt(k,399)*y(k,200)
         mat(k,159) = .500_r8*rxt(k,469)*y(k,200)
         mat(k,822) = rxt(k,291)*y(k,200)
         mat(k,704) = rxt(k,292)*y(k,200)
         mat(k,435) = rxt(k,167)*y(k,77) + rxt(k,123)*y(k,143) + rxt(k,132)*y(k,200)
         mat(k,75) = rxt(k,256)*y(k,200)
         mat(k,626) = rxt(k,257)*y(k,200)
         mat(k,860) = rxt(k,321)*y(k,200)
         mat(k,873) = rxt(k,306)*y(k,200)
         mat(k,682) = .370_r8*rxt(k,375)*y(k,144)
         mat(k,458) = .300_r8*rxt(k,366)*y(k,200)
         mat(k,387) = rxt(k,367)*y(k,200)
         mat(k,1006) = mat(k,1006) + rxt(k,372)*y(k,138) + rxt(k,373)*y(k,140) &
                      + rxt(k,369)*y(k,183) + 1.200_r8*rxt(k,370)*y(k,184)
         mat(k,253) = rxt(k,374)*y(k,200)
         mat(k,1021) = .140_r8*rxt(k,328)*y(k,144)
         mat(k,193) = .200_r8*rxt(k,330)*y(k,200)
         mat(k,392) = .500_r8*rxt(k,341)*y(k,200)
         mat(k,732) = .570_r8*rxt(k,433)*y(k,144)
         mat(k,1090) = .280_r8*rxt(k,342)*y(k,144)
         mat(k,266) = rxt(k,378)*y(k,200)
         mat(k,834) = rxt(k,379)*y(k,200)
         mat(k,1875) = mat(k,1875) + rxt(k,372)*y(k,119) + rxt(k,348)*y(k,177) &
                      + rxt(k,390)*y(k,178) + rxt(k,395)*y(k,179) + rxt(k,273) &
                      *y(k,180) + rxt(k,301)*y(k,181) + rxt(k,251)*y(k,184) &
                      + .170_r8*rxt(k,401)*y(k,185) + rxt(k,319)*y(k,186) &
                      + .250_r8*rxt(k,288)*y(k,188) + rxt(k,260)*y(k,190) &
                      + .920_r8*rxt(k,358)*y(k,191) + .920_r8*rxt(k,364)*y(k,192) &
                      + .470_r8*rxt(k,326)*y(k,193) + .400_r8*rxt(k,404)*y(k,194) &
                      + .830_r8*rxt(k,407)*y(k,196) + rxt(k,410)*y(k,201) + rxt(k,309) &
                      *y(k,202) + .900_r8*rxt(k,442)*y(k,204) + .800_r8*rxt(k,447) &
                      *y(k,205) + rxt(k,417)*y(k,206) + rxt(k,383)*y(k,207) &
                      + rxt(k,423)*y(k,208) + rxt(k,426)*y(k,209)
         mat(k,1774) = mat(k,1774) + rxt(k,245)*y(k,63) + rxt(k,373)*y(k,119) &
                      + rxt(k,359)*y(k,191) + rxt(k,365)*y(k,192) + .470_r8*rxt(k,325) &
                      *y(k,193) + rxt(k,151)*y(k,200) + rxt(k,384)*y(k,207)
         mat(k,1905) = mat(k,1905) + rxt(k,246)*y(k,63) + rxt(k,123)*y(k,97)
         mat(k,1677) = mat(k,1677) + .570_r8*rxt(k,430)*y(k,27) + .130_r8*rxt(k,269) &
                      *y(k,46) + .280_r8*rxt(k,298)*y(k,50) + .370_r8*rxt(k,375) &
                      *y(k,116) + .140_r8*rxt(k,328)*y(k,122) + .570_r8*rxt(k,433) &
                      *y(k,127) + .280_r8*rxt(k,342)*y(k,128) + rxt(k,135)*y(k,200)
         mat(k,68) = .800_r8*rxt(k,411)*y(k,200)
         mat(k,692) = rxt(k,464)*y(k,200)
         mat(k,845) = .200_r8*rxt(k,451)*y(k,200)
         mat(k,83) = .280_r8*rxt(k,419)*y(k,200)
         mat(k,104) = .380_r8*rxt(k,421)*y(k,200)
         mat(k,109) = .630_r8*rxt(k,427)*y(k,200)
         mat(k,768) = mat(k,768) + rxt(k,348)*y(k,138)
         mat(k,345) = mat(k,345) + rxt(k,390)*y(k,138)
         mat(k,302) = mat(k,302) + rxt(k,395)*y(k,138)
         mat(k,657) = mat(k,657) + rxt(k,273)*y(k,138) + 2.400_r8*rxt(k,270)*y(k,180) &
                      + rxt(k,271)*y(k,184)
         mat(k,633) = mat(k,633) + rxt(k,301)*y(k,138) + rxt(k,299)*y(k,184)
         mat(k,1194) = mat(k,1194) + rxt(k,369)*y(k,119) + .900_r8*rxt(k,282)*y(k,184) &
                      + rxt(k,355)*y(k,191) + rxt(k,360)*y(k,192) + .470_r8*rxt(k,322) &
                      *y(k,193) + rxt(k,380)*y(k,207)
         mat(k,1394) = mat(k,1394) + rxt(k,171)*y(k,80) + 1.200_r8*rxt(k,370)*y(k,119) &
                      + rxt(k,251)*y(k,138) + rxt(k,271)*y(k,180) + rxt(k,299) &
                      *y(k,181) + .900_r8*rxt(k,282)*y(k,183) + 4.000_r8*rxt(k,248) &
                      *y(k,184) + rxt(k,356)*y(k,191) + rxt(k,361)*y(k,192) &
                      + .730_r8*rxt(k,323)*y(k,193) + rxt(k,332)*y(k,195) &
                      + .500_r8*rxt(k,435)*y(k,198) + .300_r8*rxt(k,311)*y(k,203) &
                      + rxt(k,440)*y(k,204) + rxt(k,445)*y(k,205) + .800_r8*rxt(k,381) &
                      *y(k,207)
         mat(k,592) = mat(k,592) + .170_r8*rxt(k,401)*y(k,138) + .070_r8*rxt(k,400) &
                      *y(k,189)
         mat(k,429) = rxt(k,319)*y(k,138)
         mat(k,546) = mat(k,546) + .250_r8*rxt(k,288)*y(k,138)
         mat(k,1344) = mat(k,1344) + .070_r8*rxt(k,400)*y(k,185) + .160_r8*rxt(k,403) &
                      *y(k,194) + .330_r8*rxt(k,406)*y(k,196)
         mat(k,308) = mat(k,308) + rxt(k,260)*y(k,138)
         mat(k,1070) = mat(k,1070) + .920_r8*rxt(k,358)*y(k,138) + rxt(k,359)*y(k,140) &
                      + rxt(k,355)*y(k,183) + rxt(k,356)*y(k,184)
         mat(k,1146) = mat(k,1146) + .920_r8*rxt(k,364)*y(k,138) + rxt(k,365)*y(k,140) &
                      + rxt(k,360)*y(k,183) + rxt(k,361)*y(k,184)
         mat(k,1113) = mat(k,1113) + .470_r8*rxt(k,326)*y(k,138) + .470_r8*rxt(k,325) &
                      *y(k,140) + .470_r8*rxt(k,322)*y(k,183) + .730_r8*rxt(k,323) &
                      *y(k,184)
         mat(k,554) = mat(k,554) + .400_r8*rxt(k,404)*y(k,138) + .160_r8*rxt(k,403) &
                      *y(k,189)
         mat(k,1164) = mat(k,1164) + rxt(k,332)*y(k,184)
         mat(k,710) = mat(k,710) + .830_r8*rxt(k,407)*y(k,138) + .330_r8*rxt(k,406) &
                      *y(k,189)
         mat(k,929) = mat(k,929) + .500_r8*rxt(k,435)*y(k,184)
         mat(k,1560) = mat(k,1560) + .650_r8*rxt(k,388)*y(k,28) + rxt(k,212)*y(k,40) &
                      + .350_r8*rxt(k,267)*y(k,45) + rxt(k,274)*y(k,47) + rxt(k,280) &
                      *y(k,68) + rxt(k,252)*y(k,73) + rxt(k,182)*y(k,80) + rxt(k,255) &
                      *y(k,83) + .730_r8*rxt(k,399)*y(k,87) + .500_r8*rxt(k,469) &
                      *y(k,88) + rxt(k,291)*y(k,92) + rxt(k,292)*y(k,93) + rxt(k,132) &
                      *y(k,97) + rxt(k,256)*y(k,104) + rxt(k,257)*y(k,105) &
                      + rxt(k,321)*y(k,111) + rxt(k,306)*y(k,113) + .300_r8*rxt(k,366) &
                      *y(k,117) + rxt(k,367)*y(k,118) + rxt(k,374)*y(k,120) &
                      + .200_r8*rxt(k,330)*y(k,123) + .500_r8*rxt(k,341)*y(k,126) &
                      + rxt(k,378)*y(k,132) + rxt(k,379)*y(k,133) + rxt(k,151) &
                      *y(k,140) + rxt(k,135)*y(k,144) + .800_r8*rxt(k,411)*y(k,151) &
                      + rxt(k,464)*y(k,158) + .200_r8*rxt(k,451)*y(k,166) &
                      + .280_r8*rxt(k,419)*y(k,168) + .380_r8*rxt(k,421)*y(k,170) &
                      + .630_r8*rxt(k,427)*y(k,172)
         mat(k,315) = mat(k,315) + rxt(k,410)*y(k,138)
         mat(k,601) = mat(k,601) + rxt(k,309)*y(k,138)
         mat(k,943) = mat(k,943) + .300_r8*rxt(k,311)*y(k,184)
         mat(k,910) = mat(k,910) + .900_r8*rxt(k,442)*y(k,138) + rxt(k,440)*y(k,184)
         mat(k,890) = mat(k,890) + .800_r8*rxt(k,447)*y(k,138) + rxt(k,445)*y(k,184)
         mat(k,568) = mat(k,568) + rxt(k,417)*y(k,138)
         mat(k,983) = mat(k,983) + rxt(k,383)*y(k,138) + rxt(k,384)*y(k,140) &
                      + rxt(k,380)*y(k,183) + .800_r8*rxt(k,381)*y(k,184)
         mat(k,585) = mat(k,585) + rxt(k,423)*y(k,138)
         mat(k,353) = mat(k,353) + rxt(k,426)*y(k,138)
         mat(k,306) = -(rxt(k,258)*y(k,189) + rxt(k,260)*y(k,138))
         mat(k,1283) = -rxt(k,258)*y(k,190)
         mat(k,1819) = -rxt(k,260)*y(k,190)
         mat(k,1788) = rxt(k,244)*y(k,189)
         mat(k,1283) = mat(k,1283) + rxt(k,244)*y(k,63)
      end do
      end subroutine nlnmat07
      subroutine nlnmat08( avec_len, mat, y, rxt )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k,1063) = -(rxt(k,355)*y(k,183) + rxt(k,356)*y(k,184) + rxt(k,357) &
                      *y(k,189) + rxt(k,358)*y(k,138) + rxt(k,359)*y(k,140))
         mat(k,1186) = -rxt(k,355)*y(k,191)
         mat(k,1385) = -rxt(k,356)*y(k,191)
         mat(k,1334) = -rxt(k,357)*y(k,191)
         mat(k,1865) = -rxt(k,358)*y(k,191)
         mat(k,1764) = -rxt(k,359)*y(k,191)
         mat(k,678) = .600_r8*rxt(k,376)*y(k,200)
         mat(k,1550) = .600_r8*rxt(k,376)*y(k,116)
         mat(k,1141) = -(rxt(k,360)*y(k,183) + rxt(k,361)*y(k,184) + rxt(k,362) &
                      *y(k,189) + rxt(k,364)*y(k,138) + rxt(k,365)*y(k,140))
         mat(k,1189) = -rxt(k,360)*y(k,192)
         mat(k,1388) = -rxt(k,361)*y(k,192)
         mat(k,1337) = -rxt(k,362)*y(k,192)
         mat(k,1868) = -rxt(k,364)*y(k,192)
         mat(k,1767) = -rxt(k,365)*y(k,192)
         mat(k,680) = .400_r8*rxt(k,376)*y(k,200)
         mat(k,1553) = .400_r8*rxt(k,376)*y(k,116)
         mat(k,1108) = -(rxt(k,322)*y(k,183) + rxt(k,323)*y(k,184) + rxt(k,324) &
                      *y(k,189) + rxt(k,325)*y(k,140) + (rxt(k,326) + rxt(k,327) &
                      ) * y(k,138))
         mat(k,1188) = -rxt(k,322)*y(k,193)
         mat(k,1387) = -rxt(k,323)*y(k,193)
         mat(k,1336) = -rxt(k,324)*y(k,193)
         mat(k,1766) = -rxt(k,325)*y(k,193)
         mat(k,1867) = -(rxt(k,326) + rxt(k,327)) * y(k,193)
         mat(k,1017) = .500_r8*rxt(k,329)*y(k,200)
         mat(k,191) = .200_r8*rxt(k,330)*y(k,200)
         mat(k,1086) = rxt(k,343)*y(k,200)
         mat(k,1552) = .500_r8*rxt(k,329)*y(k,122) + .200_r8*rxt(k,330)*y(k,123) &
                      + rxt(k,343)*y(k,128)
         mat(k,551) = -(rxt(k,403)*y(k,189) + rxt(k,404)*y(k,138) + rxt(k,405) &
                      *y(k,139))
         mat(k,1303) = -rxt(k,403)*y(k,194)
         mat(k,1836) = -rxt(k,404)*y(k,194)
         mat(k,1701) = -rxt(k,405)*y(k,194)
         mat(k,1160) = -(rxt(k,331)*y(k,183) + rxt(k,332)*y(k,184) + rxt(k,333) &
                      *y(k,189) + 4._r8*rxt(k,334)*y(k,195) + rxt(k,335)*y(k,138) &
                      + rxt(k,336)*y(k,140) + rxt(k,344)*y(k,139))
         mat(k,1190) = -rxt(k,331)*y(k,195)
         mat(k,1389) = -rxt(k,332)*y(k,195)
         mat(k,1338) = -rxt(k,333)*y(k,195)
         mat(k,1869) = -rxt(k,335)*y(k,195)
         mat(k,1768) = -rxt(k,336)*y(k,195)
         mat(k,1712) = -rxt(k,344)*y(k,195)
         mat(k,1018) = .500_r8*rxt(k,329)*y(k,200)
         mat(k,192) = .500_r8*rxt(k,330)*y(k,200)
         mat(k,1554) = .500_r8*rxt(k,329)*y(k,122) + .500_r8*rxt(k,330)*y(k,123)
         mat(k,707) = -(rxt(k,406)*y(k,189) + rxt(k,407)*y(k,138) + rxt(k,408) &
                      *y(k,139))
         mat(k,1316) = -rxt(k,406)*y(k,196)
         mat(k,1846) = -rxt(k,407)*y(k,196)
         mat(k,1706) = -rxt(k,408)*y(k,196)
         mat(k,512) = -(rxt(k,337)*y(k,189) + rxt(k,338)*y(k,138))
         mat(k,1299) = -rxt(k,337)*y(k,197)
         mat(k,1834) = -rxt(k,338)*y(k,197)
         mat(k,364) = rxt(k,339)*y(k,200)
         mat(k,196) = rxt(k,340)*y(k,200)
         mat(k,1505) = rxt(k,339)*y(k,124) + rxt(k,340)*y(k,125)
         mat(k,924) = -(rxt(k,435)*y(k,184) + rxt(k,436)*y(k,189) + rxt(k,437) &
                      *y(k,138) + rxt(k,438)*y(k,140))
         mat(k,1378) = -rxt(k,435)*y(k,198)
         mat(k,1326) = -rxt(k,436)*y(k,198)
         mat(k,1858) = -rxt(k,437)*y(k,198)
         mat(k,1756) = -rxt(k,438)*y(k,198)
         mat(k,749) = rxt(k,429)*y(k,140)
         mat(k,728) = rxt(k,432)*y(k,140)
         mat(k,1756) = mat(k,1756) + rxt(k,429)*y(k,27) + rxt(k,432)*y(k,127) &
                      + .500_r8*rxt(k,449)*y(k,165)
         mat(k,276) = rxt(k,439)*y(k,200)
         mat(k,796) = .500_r8*rxt(k,449)*y(k,140)
         mat(k,1541) = rxt(k,439)*y(k,142)
         mat(k,1420) = -(rxt(k,114)*y(k,95) + rxt(k,115)*y(k,210) + rxt(k,118) &
                      *y(k,144) + (rxt(k,196) + rxt(k,197)) * y(k,103) + (rxt(k,219) &
                      + rxt(k,220)) * y(k,99) + rxt(k,225)*y(k,85) + rxt(k,226) &
                      *y(k,86) + rxt(k,264)*y(k,104))
         mat(k,967) = -rxt(k,114)*y(k,199)
         mat(k,1956) = -rxt(k,115)*y(k,199)
         mat(k,1679) = -rxt(k,118)*y(k,199)
         mat(k,1254) = -(rxt(k,196) + rxt(k,197)) * y(k,199)
         mat(k,647) = -(rxt(k,219) + rxt(k,220)) * y(k,199)
         mat(k,40) = -rxt(k,225)*y(k,199)
         mat(k,72) = -rxt(k,226)*y(k,199)
         mat(k,76) = -rxt(k,264)*y(k,199)
         mat(k,1563) = -(rxt(k,131)*y(k,95) + rxt(k,132)*y(k,97) + rxt(k,133)*y(k,189) &
                      + rxt(k,134)*y(k,143) + rxt(k,135)*y(k,144) + (4._r8*rxt(k,136) &
                      + 4._r8*rxt(k,137)) * y(k,200) + rxt(k,139)*y(k,108) + rxt(k,151) &
                      *y(k,140) + rxt(k,152)*y(k,129) + rxt(k,160)*y(k,139) + rxt(k,161) &
                      *y(k,107) + rxt(k,180)*y(k,81) + (rxt(k,182) + rxt(k,183) &
                      ) * y(k,80) + rxt(k,185)*y(k,103) + rxt(k,188)*y(k,110) &
                      + rxt(k,212)*y(k,40) + rxt(k,214)*y(k,99) + rxt(k,247)*y(k,63) &
                      + rxt(k,252)*y(k,73) + rxt(k,253)*y(k,74) + (rxt(k,255) &
                      + rxt(k,265)) * y(k,83) + rxt(k,256)*y(k,104) + rxt(k,257) &
                      *y(k,105) + rxt(k,267)*y(k,45) + rxt(k,274)*y(k,47) + rxt(k,275) &
                      *y(k,48) + rxt(k,277)*y(k,49) + rxt(k,279)*y(k,66) + rxt(k,280) &
                      *y(k,68) + rxt(k,285)*y(k,71) + rxt(k,286)*y(k,72) + rxt(k,291) &
                      *y(k,92) + rxt(k,292)*y(k,93) + rxt(k,293)*y(k,148) + rxt(k,294) &
                      *y(k,46) + rxt(k,302)*y(k,51) + rxt(k,303)*y(k,52) + rxt(k,305) &
                      *y(k,70) + rxt(k,306)*y(k,113) + rxt(k,307)*y(k,141) + rxt(k,310) &
                      *y(k,153) + rxt(k,314)*y(k,154) + rxt(k,315)*y(k,50) + rxt(k,316) &
                      *y(k,69) + rxt(k,318)*y(k,37) + rxt(k,321)*y(k,111) + rxt(k,329) &
                      *y(k,122) + rxt(k,330)*y(k,123) + rxt(k,339)*y(k,124) + rxt(k,340) &
                      *y(k,125) + rxt(k,341)*y(k,126) + rxt(k,343)*y(k,128) + rxt(k,346) &
                      *y(k,24) + rxt(k,350)*y(k,25) + rxt(k,351)*y(k,36) + rxt(k,352) &
                      *y(k,112) + rxt(k,353)*y(k,114) + rxt(k,354)*y(k,115) + rxt(k,366) &
                      *y(k,117) + rxt(k,367)*y(k,118) + rxt(k,374)*y(k,120) + rxt(k,376) &
                      *y(k,116) + rxt(k,377)*y(k,121) + rxt(k,378)*y(k,132) + rxt(k,379) &
                      *y(k,133) + rxt(k,385)*y(k,169) + rxt(k,388)*y(k,28) + rxt(k,391) &
                      *y(k,29) + rxt(k,392)*y(k,43) + rxt(k,394)*y(k,44) + rxt(k,398) &
                      *y(k,53) + rxt(k,399)*y(k,87) + rxt(k,411)*y(k,151) + rxt(k,414) &
                      *y(k,152) + rxt(k,418)*y(k,167) + rxt(k,419)*y(k,168) + rxt(k,421) &
                      *y(k,170) + rxt(k,424)*y(k,171) + rxt(k,427)*y(k,172) + rxt(k,428) &
                      *y(k,173) + rxt(k,431)*y(k,27) + rxt(k,434)*y(k,127) + rxt(k,439) &
                      *y(k,142) + rxt(k,443)*y(k,162) + rxt(k,444)*y(k,163) + rxt(k,448) &
                      *y(k,164) + rxt(k,450)*y(k,165) + rxt(k,451)*y(k,166) + rxt(k,453) &
                      *y(k,146) + rxt(k,458)*y(k,155) + rxt(k,463)*y(k,157) + rxt(k,464) &
                      *y(k,158) + (rxt(k,467) + rxt(k,469)) * y(k,88) + rxt(k,468) &
                      *y(k,134))
         mat(k,968) = -rxt(k,131)*y(k,200)
         mat(k,436) = -rxt(k,132)*y(k,200)
         mat(k,1347) = -rxt(k,133)*y(k,200)
         mat(k,1908) = -rxt(k,134)*y(k,200)
         mat(k,1680) = -rxt(k,135)*y(k,200)
         mat(k,240) = -rxt(k,139)*y(k,200)
         mat(k,1777) = -rxt(k,151)*y(k,200)
         mat(k,258) = -rxt(k,152)*y(k,200)
         mat(k,1721) = -rxt(k,160)*y(k,200)
         mat(k,1223) = -rxt(k,161)*y(k,200)
         mat(k,785) = -rxt(k,180)*y(k,200)
         mat(k,1623) = -(rxt(k,182) + rxt(k,183)) * y(k,200)
         mat(k,1255) = -rxt(k,185)*y(k,200)
         mat(k,666) = -rxt(k,188)*y(k,200)
         mat(k,1932) = -rxt(k,212)*y(k,200)
         mat(k,648) = -rxt(k,214)*y(k,200)
         mat(k,1800) = -rxt(k,247)*y(k,200)
         mat(k,642) = -rxt(k,252)*y(k,200)
         mat(k,289) = -rxt(k,253)*y(k,200)
         mat(k,869) = -(rxt(k,255) + rxt(k,265)) * y(k,200)
         mat(k,77) = -rxt(k,256)*y(k,200)
         mat(k,627) = -rxt(k,257)*y(k,200)
         mat(k,176) = -rxt(k,267)*y(k,200)
         mat(k,143) = -rxt(k,274)*y(k,200)
         mat(k,186) = -rxt(k,275)*y(k,200)
         mat(k,151) = -rxt(k,277)*y(k,200)
         mat(k,957) = -rxt(k,279)*y(k,200)
         mat(k,27) = -rxt(k,280)*y(k,200)
         mat(k,399) = -rxt(k,285)*y(k,200)
         mat(k,360) = -rxt(k,286)*y(k,200)
         mat(k,823) = -rxt(k,291)*y(k,200)
         mat(k,705) = -rxt(k,292)*y(k,200)
         mat(k,325) = -rxt(k,293)*y(k,200)
         mat(k,422) = -rxt(k,294)*y(k,200)
         mat(k,248) = -rxt(k,302)*y(k,200)
         mat(k,36) = -rxt(k,303)*y(k,200)
         mat(k,1032) = -rxt(k,305)*y(k,200)
         mat(k,874) = -rxt(k,306)*y(k,200)
         mat(k,699) = -rxt(k,307)*y(k,200)
         mat(k,406) = -rxt(k,310)*y(k,200)
         mat(k,283) = -rxt(k,314)*y(k,200)
         mat(k,812) = -rxt(k,315)*y(k,200)
         mat(k,778) = -rxt(k,316)*y(k,200)
         mat(k,228) = -rxt(k,318)*y(k,200)
         mat(k,862) = -rxt(k,321)*y(k,200)
         mat(k,1022) = -rxt(k,329)*y(k,200)
         mat(k,194) = -rxt(k,330)*y(k,200)
         mat(k,367) = -rxt(k,339)*y(k,200)
         mat(k,199) = -rxt(k,340)*y(k,200)
         mat(k,393) = -rxt(k,341)*y(k,200)
         mat(k,1092) = -rxt(k,343)*y(k,200)
         mat(k,484) = -rxt(k,346)*y(k,200)
         mat(k,509) = -rxt(k,350)*y(k,200)
         mat(k,127) = -rxt(k,351)*y(k,200)
         mat(k,124) = -rxt(k,352)*y(k,200)
         mat(k,189) = -rxt(k,353)*y(k,200)
         mat(k,49) = -rxt(k,354)*y(k,200)
         mat(k,459) = -rxt(k,366)*y(k,200)
         mat(k,388) = -rxt(k,367)*y(k,200)
         mat(k,254) = -rxt(k,374)*y(k,200)
         mat(k,684) = -rxt(k,376)*y(k,200)
         mat(k,525) = -rxt(k,377)*y(k,200)
         mat(k,267) = -rxt(k,378)*y(k,200)
         mat(k,836) = -rxt(k,379)*y(k,200)
         mat(k,96) = -rxt(k,385)*y(k,200)
         mat(k,60) = -rxt(k,388)*y(k,200)
         mat(k,273) = -rxt(k,391)*y(k,200)
         mat(k,116) = -rxt(k,392)*y(k,200)
         mat(k,215) = -rxt(k,394)*y(k,200)
         mat(k,134) = -rxt(k,398)*y(k,200)
         mat(k,89) = -rxt(k,399)*y(k,200)
         mat(k,69) = -rxt(k,411)*y(k,200)
         mat(k,209) = -rxt(k,414)*y(k,200)
         mat(k,499) = -rxt(k,418)*y(k,200)
         mat(k,84) = -rxt(k,419)*y(k,200)
         mat(k,105) = -rxt(k,421)*y(k,200)
         mat(k,541) = -rxt(k,424)*y(k,200)
         mat(k,110) = -rxt(k,427)*y(k,200)
         mat(k,297) = -rxt(k,428)*y(k,200)
         mat(k,754) = -rxt(k,431)*y(k,200)
         mat(k,733) = -rxt(k,434)*y(k,200)
         mat(k,278) = -rxt(k,439)*y(k,200)
         mat(k,451) = -rxt(k,443)*y(k,200)
         mat(k,466) = -rxt(k,444)*y(k,200)
         mat(k,338) = -rxt(k,448)*y(k,200)
         mat(k,798) = -rxt(k,450)*y(k,200)
         mat(k,847) = -rxt(k,451)*y(k,200)
         mat(k,222) = -rxt(k,453)*y(k,200)
         mat(k,371) = -rxt(k,458)*y(k,200)
         mat(k,1042) = -rxt(k,463)*y(k,200)
         mat(k,693) = -rxt(k,464)*y(k,200)
         mat(k,160) = -(rxt(k,467) + rxt(k,469)) * y(k,200)
         mat(k,32) = -rxt(k,468)*y(k,200)
         mat(k,754) = mat(k,754) + .630_r8*rxt(k,430)*y(k,144)
         mat(k,176) = mat(k,176) + .650_r8*rxt(k,267)*y(k,200)
         mat(k,422) = mat(k,422) + .130_r8*rxt(k,269)*y(k,144)
         mat(k,186) = mat(k,186) + .500_r8*rxt(k,275)*y(k,200)
         mat(k,812) = mat(k,812) + .360_r8*rxt(k,298)*y(k,144)
         mat(k,1800) = mat(k,1800) + rxt(k,246)*y(k,143)
         mat(k,289) = mat(k,289) + .300_r8*rxt(k,253)*y(k,200)
         mat(k,1597) = rxt(k,169)*y(k,189)
         mat(k,622) = rxt(k,223)*y(k,210)
         mat(k,1236) = rxt(k,130)*y(k,144) + 2.000_r8*rxt(k,125)*y(k,189)
         mat(k,968) = mat(k,968) + rxt(k,122)*y(k,143) + rxt(k,114)*y(k,199)
         mat(k,436) = mat(k,436) + rxt(k,123)*y(k,143)
         mat(k,648) = mat(k,648) + rxt(k,213)*y(k,143) + rxt(k,219)*y(k,199)
         mat(k,1255) = mat(k,1255) + rxt(k,184)*y(k,143) + rxt(k,196)*y(k,199)
         mat(k,77) = mat(k,77) + rxt(k,264)*y(k,199)
         mat(k,611) = rxt(k,215)*y(k,143)
         mat(k,666) = mat(k,666) + rxt(k,187)*y(k,143)
         mat(k,684) = mat(k,684) + .320_r8*rxt(k,375)*y(k,144)
         mat(k,525) = mat(k,525) + .600_r8*rxt(k,377)*y(k,200)
         mat(k,1022) = mat(k,1022) + .240_r8*rxt(k,328)*y(k,144)
         mat(k,194) = mat(k,194) + .100_r8*rxt(k,330)*y(k,200)
         mat(k,733) = mat(k,733) + .630_r8*rxt(k,433)*y(k,144)
         mat(k,1092) = mat(k,1092) + .360_r8*rxt(k,342)*y(k,144)
         mat(k,1878) = rxt(k,153)*y(k,189)
         mat(k,1777) = mat(k,1777) + rxt(k,148)*y(k,189)
         mat(k,1908) = mat(k,1908) + rxt(k,246)*y(k,63) + rxt(k,122)*y(k,95) &
                      + rxt(k,123)*y(k,97) + rxt(k,213)*y(k,99) + rxt(k,184)*y(k,103) &
                      + rxt(k,215)*y(k,109) + rxt(k,187)*y(k,110) + rxt(k,128) &
                      *y(k,189)
         mat(k,1680) = mat(k,1680) + .630_r8*rxt(k,430)*y(k,27) + .130_r8*rxt(k,269) &
                      *y(k,46) + .360_r8*rxt(k,298)*y(k,50) + rxt(k,130)*y(k,94) &
                      + .320_r8*rxt(k,375)*y(k,116) + .240_r8*rxt(k,328)*y(k,122) &
                      + .630_r8*rxt(k,433)*y(k,127) + .360_r8*rxt(k,342)*y(k,128) &
                      + rxt(k,129)*y(k,189)
         mat(k,406) = mat(k,406) + .500_r8*rxt(k,310)*y(k,200)
         mat(k,96) = mat(k,96) + .500_r8*rxt(k,385)*y(k,200)
         mat(k,378) = .400_r8*rxt(k,386)*y(k,189)
         mat(k,1196) = .450_r8*rxt(k,283)*y(k,189)
         mat(k,594) = .400_r8*rxt(k,400)*y(k,189)
         mat(k,1347) = mat(k,1347) + rxt(k,169)*y(k,77) + 2.000_r8*rxt(k,125)*y(k,94) &
                      + rxt(k,153)*y(k,138) + rxt(k,148)*y(k,140) + rxt(k,128) &
                      *y(k,143) + rxt(k,129)*y(k,144) + .400_r8*rxt(k,386)*y(k,176) &
                      + .450_r8*rxt(k,283)*y(k,183) + .400_r8*rxt(k,400)*y(k,185) &
                      + .450_r8*rxt(k,333)*y(k,195) + .400_r8*rxt(k,406)*y(k,196) &
                      + .200_r8*rxt(k,337)*y(k,197) + .150_r8*rxt(k,312)*y(k,203)
         mat(k,1166) = .450_r8*rxt(k,333)*y(k,189)
         mat(k,712) = .400_r8*rxt(k,406)*y(k,189)
         mat(k,516) = .200_r8*rxt(k,337)*y(k,189)
         mat(k,1421) = rxt(k,114)*y(k,95) + rxt(k,219)*y(k,99) + rxt(k,196)*y(k,103) &
                      + rxt(k,264)*y(k,104) + 2.000_r8*rxt(k,115)*y(k,210)
         mat(k,1563) = mat(k,1563) + .650_r8*rxt(k,267)*y(k,45) + .500_r8*rxt(k,275) &
                      *y(k,48) + .300_r8*rxt(k,253)*y(k,74) + .600_r8*rxt(k,377) &
                      *y(k,121) + .100_r8*rxt(k,330)*y(k,123) + .500_r8*rxt(k,310) &
                      *y(k,153) + .500_r8*rxt(k,385)*y(k,169)
         mat(k,945) = .150_r8*rxt(k,312)*y(k,189)
         mat(k,1957) = rxt(k,223)*y(k,91) + 2.000_r8*rxt(k,115)*y(k,199)
      end do
      end subroutine nlnmat08
      subroutine nlnmat09( avec_len, mat, y, rxt )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k,313) = -(rxt(k,409)*y(k,189) + rxt(k,410)*y(k,138))
         mat(k,1284) = -rxt(k,409)*y(k,201)
         mat(k,1820) = -rxt(k,410)*y(k,201)
         mat(k,86) = .200_r8*rxt(k,399)*y(k,200)
         mat(k,66) = .140_r8*rxt(k,411)*y(k,200)
         mat(k,206) = rxt(k,414)*y(k,200)
         mat(k,1479) = .200_r8*rxt(k,399)*y(k,87) + .140_r8*rxt(k,411)*y(k,151) &
                      + rxt(k,414)*y(k,152)
         mat(k,598) = -(rxt(k,308)*y(k,189) + rxt(k,309)*y(k,138))
         mat(k,1307) = -rxt(k,308)*y(k,202)
         mat(k,1840) = -rxt(k,309)*y(k,202)
         mat(k,801) = rxt(k,315)*y(k,200)
         mat(k,402) = .500_r8*rxt(k,310)*y(k,200)
         mat(k,1513) = rxt(k,315)*y(k,50) + .500_r8*rxt(k,310)*y(k,153)
         mat(k,940) = -(rxt(k,311)*y(k,184) + rxt(k,312)*y(k,189) + rxt(k,313) &
                      *y(k,138))
         mat(k,1379) = -rxt(k,311)*y(k,203)
         mat(k,1327) = -rxt(k,312)*y(k,203)
         mat(k,1859) = -rxt(k,313)*y(k,203)
         mat(k,750) = .060_r8*rxt(k,430)*y(k,144)
         mat(k,775) = rxt(k,316)*y(k,200)
         mat(k,729) = .060_r8*rxt(k,433)*y(k,144)
         mat(k,1661) = .060_r8*rxt(k,430)*y(k,27) + .060_r8*rxt(k,433)*y(k,127)
         mat(k,281) = rxt(k,314)*y(k,200)
         mat(k,843) = .150_r8*rxt(k,451)*y(k,200)
         mat(k,1542) = rxt(k,316)*y(k,69) + rxt(k,314)*y(k,154) + .150_r8*rxt(k,451) &
                      *y(k,166)
         mat(k,905) = -(rxt(k,440)*y(k,184) + rxt(k,441)*y(k,189) + rxt(k,442) &
                      *y(k,138))
         mat(k,1377) = -rxt(k,440)*y(k,204)
         mat(k,1325) = -rxt(k,441)*y(k,204)
         mat(k,1857) = -rxt(k,442)*y(k,204)
         mat(k,1755) = .500_r8*rxt(k,449)*y(k,165)
         mat(k,449) = rxt(k,443)*y(k,200)
         mat(k,795) = .500_r8*rxt(k,449)*y(k,140) + rxt(k,450)*y(k,200)
         mat(k,1540) = rxt(k,443)*y(k,162) + rxt(k,450)*y(k,165)
         mat(k,883) = -(rxt(k,445)*y(k,184) + rxt(k,446)*y(k,189) + rxt(k,447) &
                      *y(k,138))
         mat(k,1376) = -rxt(k,445)*y(k,205)
         mat(k,1324) = -rxt(k,446)*y(k,205)
         mat(k,1856) = -rxt(k,447)*y(k,205)
         mat(k,748) = rxt(k,431)*y(k,200)
         mat(k,727) = rxt(k,434)*y(k,200)
         mat(k,336) = rxt(k,448)*y(k,200)
         mat(k,1539) = rxt(k,431)*y(k,27) + rxt(k,434)*y(k,127) + rxt(k,448)*y(k,164)
         mat(k,562) = -(rxt(k,416)*y(k,189) + rxt(k,417)*y(k,138))
         mat(k,1304) = -rxt(k,416)*y(k,206)
         mat(k,1837) = -rxt(k,417)*y(k,206)
         mat(k,492) = rxt(k,418)*y(k,200)
         mat(k,82) = .650_r8*rxt(k,419)*y(k,200)
         mat(k,1510) = rxt(k,418)*y(k,167) + .650_r8*rxt(k,419)*y(k,168)
         mat(k,979) = -(rxt(k,380)*y(k,183) + rxt(k,381)*y(k,184) + rxt(k,382) &
                      *y(k,189) + rxt(k,383)*y(k,138) + rxt(k,384)*y(k,140))
         mat(k,1182) = -rxt(k,380)*y(k,207)
         mat(k,1381) = -rxt(k,381)*y(k,207)
         mat(k,1330) = -rxt(k,382)*y(k,207)
         mat(k,1861) = -rxt(k,383)*y(k,207)
         mat(k,1759) = -rxt(k,384)*y(k,207)
         mat(k,122) = rxt(k,352)*y(k,200)
         mat(k,188) = rxt(k,353)*y(k,200)
         mat(k,48) = rxt(k,354)*y(k,200)
         mat(k,521) = .400_r8*rxt(k,377)*y(k,200)
         mat(k,95) = .500_r8*rxt(k,385)*y(k,200)
         mat(k,1545) = rxt(k,352)*y(k,112) + rxt(k,353)*y(k,114) + rxt(k,354)*y(k,115) &
                      + .400_r8*rxt(k,377)*y(k,121) + .500_r8*rxt(k,385)*y(k,169)
         mat(k,578) = -(rxt(k,422)*y(k,189) + rxt(k,423)*y(k,138))
         mat(k,1305) = -rxt(k,422)*y(k,208)
         mat(k,1838) = -rxt(k,423)*y(k,208)
         mat(k,101) = .560_r8*rxt(k,421)*y(k,200)
         mat(k,533) = rxt(k,424)*y(k,200)
         mat(k,1511) = .560_r8*rxt(k,421)*y(k,170) + rxt(k,424)*y(k,171)
         mat(k,350) = -(rxt(k,425)*y(k,189) + rxt(k,426)*y(k,138))
         mat(k,1289) = -rxt(k,425)*y(k,209)
         mat(k,1824) = -rxt(k,426)*y(k,209)
         mat(k,108) = .300_r8*rxt(k,427)*y(k,200)
         mat(k,293) = rxt(k,428)*y(k,200)
         mat(k,1485) = .300_r8*rxt(k,427)*y(k,172) + rxt(k,428)*y(k,173)
         mat(k,1967) = -(rxt(k,115)*y(k,199) + rxt(k,223)*y(k,91) + rxt(k,465) &
                      *y(k,159))
         mat(k,1431) = -rxt(k,115)*y(k,210)
         mat(k,624) = -rxt(k,223)*y(k,210)
         mat(k,139) = -rxt(k,465)*y(k,210)
         mat(k,153) = rxt(k,277)*y(k,200)
         mat(k,249) = rxt(k,302)*y(k,200)
         mat(k,37) = rxt(k,303)*y(k,200)
         mat(k,1810) = rxt(k,247)*y(k,200)
         mat(k,959) = rxt(k,279)*y(k,200)
         mat(k,779) = rxt(k,316)*y(k,200)
         mat(k,1034) = rxt(k,305)*y(k,200)
         mat(k,400) = rxt(k,285)*y(k,200)
         mat(k,362) = rxt(k,286)*y(k,200)
         mat(k,291) = rxt(k,253)*y(k,200)
         mat(k,1242) = rxt(k,126)*y(k,189)
         mat(k,972) = rxt(k,131)*y(k,200)
         mat(k,439) = rxt(k,132)*y(k,200)
         mat(k,651) = rxt(k,214)*y(k,200)
         mat(k,1264) = (rxt(k,491)+rxt(k,496))*y(k,109) + (rxt(k,484)+rxt(k,490) &
                       +rxt(k,495))*y(k,110) + rxt(k,185)*y(k,200)
         mat(k,628) = rxt(k,257)*y(k,200)
         mat(k,1228) = rxt(k,161)*y(k,200)
         mat(k,243) = rxt(k,139)*y(k,200)
         mat(k,615) = (rxt(k,491)+rxt(k,496))*y(k,103)
         mat(k,670) = (rxt(k,484)+rxt(k,490)+rxt(k,495))*y(k,103) + rxt(k,188) &
                      *y(k,200)
         mat(k,1025) = .500_r8*rxt(k,329)*y(k,200)
         mat(k,33) = rxt(k,468)*y(k,200)
         mat(k,408) = rxt(k,310)*y(k,200)
         mat(k,285) = rxt(k,314)*y(k,200)
         mat(k,1357) = rxt(k,126)*y(k,94) + rxt(k,133)*y(k,200)
         mat(k,1573) = rxt(k,277)*y(k,49) + rxt(k,302)*y(k,51) + rxt(k,303)*y(k,52) &
                      + rxt(k,247)*y(k,63) + rxt(k,279)*y(k,66) + rxt(k,316)*y(k,69) &
                      + rxt(k,305)*y(k,70) + rxt(k,285)*y(k,71) + rxt(k,286)*y(k,72) &
                      + rxt(k,253)*y(k,74) + rxt(k,131)*y(k,95) + rxt(k,132)*y(k,97) &
                      + rxt(k,214)*y(k,99) + rxt(k,185)*y(k,103) + rxt(k,257)*y(k,105) &
                      + rxt(k,161)*y(k,107) + rxt(k,139)*y(k,108) + rxt(k,188) &
                      *y(k,110) + .500_r8*rxt(k,329)*y(k,122) + rxt(k,468)*y(k,134) &
                      + rxt(k,310)*y(k,153) + rxt(k,314)*y(k,154) + rxt(k,133) &
                      *y(k,189) + 2.000_r8*rxt(k,136)*y(k,200)
      end do
      end subroutine nlnmat09
      subroutine nlnmat_finit( avec_len, mat, lmat, dti )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: dti(veclen)
      real(r8), intent(in) :: lmat(veclen,nzcnt)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
!----------------------------------------------
! ... local variables
!----------------------------------------------
      integer :: k
!----------------------------------------------
! ... complete matrix entries implicit species
!----------------------------------------------
      do k = 1,avec_len
         mat(k, 1) = lmat(k, 1)
         mat(k, 2) = lmat(k, 2)
         mat(k, 3) = lmat(k, 3)
         mat(k, 4) = lmat(k, 4)
         mat(k, 5) = lmat(k, 5)
         mat(k, 6) = lmat(k, 6)
         mat(k, 7) = lmat(k, 7)
         mat(k, 8) = lmat(k, 8)
         mat(k, 9) = lmat(k, 9)
         mat(k, 10) = lmat(k, 10)
         mat(k, 11) = lmat(k, 11)
         mat(k, 12) = lmat(k, 12)
         mat(k, 13) = lmat(k, 13)
         mat(k, 14) = lmat(k, 14)
         mat(k, 15) = lmat(k, 15)
         mat(k, 16) = lmat(k, 16)
         mat(k, 17) = lmat(k, 17)
         mat(k, 18) = lmat(k, 18)
         mat(k, 19) = lmat(k, 19)
         mat(k, 20) = lmat(k, 20)
         mat(k, 21) = lmat(k, 21)
         mat(k, 22) = lmat(k, 22)
         mat(k, 23) = lmat(k, 23)
         mat(k, 24) = lmat(k, 24)
         mat(k, 25) = mat(k, 25) + lmat(k, 25)
         mat(k, 28) = lmat(k, 28)
         mat(k, 29) = lmat(k, 29)
         mat(k, 30) = lmat(k, 30)
         mat(k, 31) = mat(k, 31) + lmat(k, 31)
         mat(k, 34) = mat(k, 34) + lmat(k, 34)
         mat(k, 38) = mat(k, 38) + lmat(k, 38)
         mat(k, 39) = mat(k, 39) + lmat(k, 39)
         mat(k, 41) = lmat(k, 41)
         mat(k, 42) = lmat(k, 42)
         mat(k, 43) = lmat(k, 43)
         mat(k, 44) = lmat(k, 44)
         mat(k, 45) = lmat(k, 45)
         mat(k, 46) = lmat(k, 46)
         mat(k, 47) = mat(k, 47) + lmat(k, 47)
         mat(k, 50) = lmat(k, 50)
         mat(k, 51) = lmat(k, 51)
         mat(k, 52) = lmat(k, 52)
         mat(k, 53) = lmat(k, 53)
         mat(k, 54) = lmat(k, 54)
         mat(k, 55) = mat(k, 55) + lmat(k, 55)
         mat(k, 61) = lmat(k, 61)
         mat(k, 62) = lmat(k, 62)
         mat(k, 63) = lmat(k, 63)
         mat(k, 64) = lmat(k, 64)
         mat(k, 65) = mat(k, 65) + lmat(k, 65)
         mat(k, 70) = mat(k, 70) + lmat(k, 70)
         mat(k, 71) = mat(k, 71) + lmat(k, 71)
         mat(k, 73) = mat(k, 73) + lmat(k, 73)
         mat(k, 74) = mat(k, 74) + lmat(k, 74)
         mat(k, 78) = mat(k, 78) + lmat(k, 78)
         mat(k, 85) = mat(k, 85) + lmat(k, 85)
         mat(k, 90) = lmat(k, 90)
         mat(k, 91) = lmat(k, 91)
         mat(k, 92) = lmat(k, 92)
         mat(k, 93) = lmat(k, 93)
         mat(k, 94) = mat(k, 94) + lmat(k, 94)
         mat(k, 96) = mat(k, 96) + lmat(k, 96)
         mat(k, 98) = mat(k, 98) + lmat(k, 98)
         mat(k, 106) = mat(k, 106) + lmat(k, 106)
         mat(k, 111) = lmat(k, 111)
         mat(k, 112) = lmat(k, 112)
         mat(k, 113) = lmat(k, 113)
         mat(k, 114) = mat(k, 114) + lmat(k, 114)
         mat(k, 117) = lmat(k, 117)
         mat(k, 118) = lmat(k, 118)
         mat(k, 119) = lmat(k, 119)
         mat(k, 120) = mat(k, 120) + lmat(k, 120)
         mat(k, 121) = lmat(k, 121)
         mat(k, 123) = lmat(k, 123)
         mat(k, 124) = mat(k, 124) + lmat(k, 124)
         mat(k, 125) = mat(k, 125) + lmat(k, 125)
         mat(k, 128) = lmat(k, 128)
         mat(k, 129) = lmat(k, 129)
         mat(k, 130) = lmat(k, 130)
         mat(k, 131) = mat(k, 131) + lmat(k, 131)
         mat(k, 132) = lmat(k, 132)
         mat(k, 134) = mat(k, 134) + lmat(k, 134)
         mat(k, 136) = mat(k, 136) + lmat(k, 136)
         mat(k, 137) = lmat(k, 137)
         mat(k, 138) = lmat(k, 138)
         mat(k, 140) = mat(k, 140) + lmat(k, 140)
         mat(k, 144) = lmat(k, 144)
         mat(k, 145) = lmat(k, 145)
         mat(k, 146) = lmat(k, 146)
         mat(k, 147) = lmat(k, 147)
         mat(k, 148) = mat(k, 148) + lmat(k, 148)
         mat(k, 156) = mat(k, 156) + lmat(k, 156)
         mat(k, 162) = lmat(k, 162)
         mat(k, 163) = lmat(k, 163)
         mat(k, 164) = lmat(k, 164)
         mat(k, 165) = lmat(k, 165)
         mat(k, 166) = lmat(k, 166)
         mat(k, 167) = lmat(k, 167)
         mat(k, 168) = lmat(k, 168)
         mat(k, 169) = lmat(k, 169)
         mat(k, 170) = lmat(k, 170)
         mat(k, 171) = mat(k, 171) + lmat(k, 171)
         mat(k, 177) = lmat(k, 177)
         mat(k, 178) = lmat(k, 178)
         mat(k, 179) = lmat(k, 179)
         mat(k, 180) = lmat(k, 180)
         mat(k, 181) = lmat(k, 181)
         mat(k, 182) = mat(k, 182) + lmat(k, 182)
         mat(k, 184) = mat(k, 184) + lmat(k, 184)
         mat(k, 185) = lmat(k, 185)
         mat(k, 186) = mat(k, 186) + lmat(k, 186)
         mat(k, 187) = mat(k, 187) + lmat(k, 187)
         mat(k, 190) = mat(k, 190) + lmat(k, 190)
         mat(k, 195) = mat(k, 195) + lmat(k, 195)
         mat(k, 197) = lmat(k, 197)
         mat(k, 198) = lmat(k, 198)
         mat(k, 199) = mat(k, 199) + lmat(k, 199)
         mat(k, 200) = mat(k, 200) + lmat(k, 200)
         mat(k, 203) = mat(k, 203) + lmat(k, 203)
         mat(k, 204) = lmat(k, 204)
         mat(k, 205) = mat(k, 205) + lmat(k, 205)
         mat(k, 207) = lmat(k, 207)
         mat(k, 208) = lmat(k, 208)
         mat(k, 209) = mat(k, 209) + lmat(k, 209)
         mat(k, 210) = lmat(k, 210)
         mat(k, 211) = mat(k, 211) + lmat(k, 211)
         mat(k, 214) = lmat(k, 214)
         mat(k, 215) = mat(k, 215) + lmat(k, 215)
         mat(k, 216) = mat(k, 216) + lmat(k, 216)
         mat(k, 217) = lmat(k, 217)
         mat(k, 219) = mat(k, 219) + lmat(k, 219)
         mat(k, 224) = mat(k, 224) + lmat(k, 224)
         mat(k, 232) = lmat(k, 232)
         mat(k, 233) = lmat(k, 233)
         mat(k, 234) = lmat(k, 234)
         mat(k, 235) = lmat(k, 235)
         mat(k, 236) = lmat(k, 236)
         mat(k, 237) = lmat(k, 237)
         mat(k, 238) = mat(k, 238) + lmat(k, 238)
         mat(k, 239) = lmat(k, 239)
         mat(k, 240) = mat(k, 240) + lmat(k, 240)
         mat(k, 241) = mat(k, 241) + lmat(k, 241)
         mat(k, 242) = lmat(k, 242)
         mat(k, 244) = mat(k, 244) + lmat(k, 244)
         mat(k, 246) = lmat(k, 246)
         mat(k, 247) = lmat(k, 247)
         mat(k, 248) = mat(k, 248) + lmat(k, 248)
         mat(k, 250) = mat(k, 250) + lmat(k, 250)
         mat(k, 251) = lmat(k, 251)
         mat(k, 253) = mat(k, 253) + lmat(k, 253)
         mat(k, 255) = lmat(k, 255)
         mat(k, 256) = mat(k, 256) + lmat(k, 256)
         mat(k, 260) = mat(k, 260) + lmat(k, 260)
         mat(k, 261) = mat(k, 261) + lmat(k, 261)
         mat(k, 262) = mat(k, 262) + lmat(k, 262)
         mat(k, 265) = lmat(k, 265)
         mat(k, 268) = mat(k, 268) + lmat(k, 268)
         mat(k, 269) = lmat(k, 269)
         mat(k, 271) = lmat(k, 271)
         mat(k, 272) = lmat(k, 272)
         mat(k, 273) = mat(k, 273) + lmat(k, 273)
         mat(k, 274) = mat(k, 274) + lmat(k, 274)
         mat(k, 275) = lmat(k, 275)
         mat(k, 277) = lmat(k, 277)
         mat(k, 278) = mat(k, 278) + lmat(k, 278)
         mat(k, 279) = lmat(k, 279)
         mat(k, 280) = mat(k, 280) + lmat(k, 280)
         mat(k, 282) = lmat(k, 282)
         mat(k, 283) = mat(k, 283) + lmat(k, 283)
         mat(k, 284) = lmat(k, 284)
         mat(k, 286) = mat(k, 286) + lmat(k, 286)
         mat(k, 287) = lmat(k, 287)
         mat(k, 289) = mat(k, 289) + lmat(k, 289)
         mat(k, 290) = mat(k, 290) + lmat(k, 290)
         mat(k, 292) = mat(k, 292) + lmat(k, 292)
         mat(k, 294) = lmat(k, 294)
         mat(k, 295) = lmat(k, 295)
         mat(k, 296) = lmat(k, 296)
         mat(k, 297) = mat(k, 297) + lmat(k, 297)
         mat(k, 300) = mat(k, 300) + lmat(k, 300)
         mat(k, 306) = mat(k, 306) + lmat(k, 306)
         mat(k, 308) = mat(k, 308) + lmat(k, 308)
         mat(k, 310) = lmat(k, 310)
         mat(k, 313) = mat(k, 313) + lmat(k, 313)
         mat(k, 319) = lmat(k, 319)
         mat(k, 320) = lmat(k, 320)
         mat(k, 321) = lmat(k, 321)
         mat(k, 322) = mat(k, 322) + lmat(k, 322)
         mat(k, 323) = lmat(k, 323)
         mat(k, 324) = lmat(k, 324)
         mat(k, 326) = lmat(k, 326)
         mat(k, 327) = mat(k, 327) + lmat(k, 327)
         mat(k, 329) = mat(k, 329) + lmat(k, 329)
         mat(k, 333) = mat(k, 333) + lmat(k, 333)
         mat(k, 334) = lmat(k, 334)
         mat(k, 335) = lmat(k, 335)
         mat(k, 337) = lmat(k, 337)
         mat(k, 338) = mat(k, 338) + lmat(k, 338)
         mat(k, 339) = lmat(k, 339)
         mat(k, 342) = mat(k, 342) + lmat(k, 342)
         mat(k, 350) = mat(k, 350) + lmat(k, 350)
         mat(k, 357) = mat(k, 357) + lmat(k, 357)
         mat(k, 359) = lmat(k, 359)
         mat(k, 360) = mat(k, 360) + lmat(k, 360)
         mat(k, 363) = mat(k, 363) + lmat(k, 363)
         mat(k, 365) = lmat(k, 365)
         mat(k, 366) = lmat(k, 366)
         mat(k, 368) = mat(k, 368) + lmat(k, 368)
         mat(k, 369) = mat(k, 369) + lmat(k, 369)
         mat(k, 373) = lmat(k, 373)
         mat(k, 375) = mat(k, 375) + lmat(k, 375)
         mat(k, 381) = mat(k, 381) + lmat(k, 381)
         mat(k, 386) = lmat(k, 386)
         mat(k, 389) = mat(k, 389) + lmat(k, 389)
         mat(k, 391) = lmat(k, 391)
         mat(k, 394) = lmat(k, 394)
         mat(k, 397) = mat(k, 397) + lmat(k, 397)
         mat(k, 401) = mat(k, 401) + lmat(k, 401)
         mat(k, 404) = lmat(k, 404)
         mat(k, 405) = lmat(k, 405)
         mat(k, 406) = mat(k, 406) + lmat(k, 406)
         mat(k, 407) = lmat(k, 407)
         mat(k, 409) = mat(k, 409) + lmat(k, 409)
         mat(k, 410) = lmat(k, 410)
         mat(k, 411) = lmat(k, 411)
         mat(k, 412) = lmat(k, 412)
         mat(k, 413) = lmat(k, 413)
         mat(k, 414) = mat(k, 414) + lmat(k, 414)
         mat(k, 416) = mat(k, 416) + lmat(k, 416)
         mat(k, 417) = mat(k, 417) + lmat(k, 417)
         mat(k, 425) = mat(k, 425) + lmat(k, 425)
         mat(k, 433) = mat(k, 433) + lmat(k, 433)
         mat(k, 436) = mat(k, 436) + lmat(k, 436)
         mat(k, 440) = lmat(k, 440)
         mat(k, 441) = lmat(k, 441)
         mat(k, 442) = lmat(k, 442)
         mat(k, 443) = lmat(k, 443)
         mat(k, 444) = mat(k, 444) + lmat(k, 444)
         mat(k, 445) = lmat(k, 445)
         mat(k, 446) = lmat(k, 446)
         mat(k, 447) = lmat(k, 447)
         mat(k, 448) = lmat(k, 448)
         mat(k, 450) = lmat(k, 450)
         mat(k, 451) = mat(k, 451) + lmat(k, 451)
         mat(k, 452) = lmat(k, 452)
         mat(k, 453) = mat(k, 453) + lmat(k, 453)
         mat(k, 457) = lmat(k, 457)
         mat(k, 462) = mat(k, 462) + lmat(k, 462)
         mat(k, 463) = mat(k, 463) + lmat(k, 463)
         mat(k, 464) = lmat(k, 464)
         mat(k, 465) = lmat(k, 465)
         mat(k, 467) = mat(k, 467) + lmat(k, 467)
         mat(k, 470) = mat(k, 470) + lmat(k, 470)
         mat(k, 476) = lmat(k, 476)
         mat(k, 477) = mat(k, 477) + lmat(k, 477)
         mat(k, 480) = mat(k, 480) + lmat(k, 480)
         mat(k, 481) = mat(k, 481) + lmat(k, 481)
         mat(k, 483) = lmat(k, 483)
         mat(k, 485) = mat(k, 485) + lmat(k, 485)
         mat(k, 486) = mat(k, 486) + lmat(k, 486)
         mat(k, 487) = lmat(k, 487)
         mat(k, 488) = lmat(k, 488)
         mat(k, 489) = lmat(k, 489)
         mat(k, 490) = mat(k, 490) + lmat(k, 490)
         mat(k, 494) = lmat(k, 494)
         mat(k, 497) = lmat(k, 497)
         mat(k, 498) = lmat(k, 498)
         mat(k, 499) = mat(k, 499) + lmat(k, 499)
         mat(k, 500) = lmat(k, 500)
         mat(k, 501) = mat(k, 501) + lmat(k, 501)
         mat(k, 505) = lmat(k, 505)
         mat(k, 506) = lmat(k, 506)
         mat(k, 508) = lmat(k, 508)
         mat(k, 509) = mat(k, 509) + lmat(k, 509)
         mat(k, 510) = lmat(k, 510)
         mat(k, 512) = mat(k, 512) + lmat(k, 512)
         mat(k, 520) = mat(k, 520) + lmat(k, 520)
         mat(k, 522) = lmat(k, 522)
         mat(k, 523) = lmat(k, 523)
         mat(k, 524) = lmat(k, 524)
         mat(k, 526) = lmat(k, 526)
         mat(k, 527) = lmat(k, 527)
         mat(k, 528) = lmat(k, 528)
         mat(k, 529) = lmat(k, 529)
         mat(k, 530) = lmat(k, 530)
         mat(k, 531) = mat(k, 531) + lmat(k, 531)
         mat(k, 535) = lmat(k, 535)
         mat(k, 538) = lmat(k, 538)
         mat(k, 540) = lmat(k, 540)
         mat(k, 541) = mat(k, 541) + lmat(k, 541)
         mat(k, 544) = mat(k, 544) + lmat(k, 544)
         mat(k, 551) = mat(k, 551) + lmat(k, 551)
         mat(k, 562) = mat(k, 562) + lmat(k, 562)
         mat(k, 578) = mat(k, 578) + lmat(k, 578)
         mat(k, 589) = mat(k, 589) + lmat(k, 589)
         mat(k, 598) = mat(k, 598) + lmat(k, 598)
         mat(k, 608) = mat(k, 608) + lmat(k, 608)
         mat(k, 609) = lmat(k, 609)
         mat(k, 611) = mat(k, 611) + lmat(k, 611)
         mat(k, 616) = mat(k, 616) + lmat(k, 616)
         mat(k, 617) = mat(k, 617) + lmat(k, 617)
         mat(k, 621) = lmat(k, 621)
         mat(k, 625) = mat(k, 625) + lmat(k, 625)
         mat(k, 630) = mat(k, 630) + lmat(k, 630)
         mat(k, 640) = mat(k, 640) + lmat(k, 640)
         mat(k, 644) = mat(k, 644) + lmat(k, 644)
         mat(k, 645) = mat(k, 645) + lmat(k, 645)
         mat(k, 646) = mat(k, 646) + lmat(k, 646)
         mat(k, 655) = mat(k, 655) + lmat(k, 655)
         mat(k, 664) = mat(k, 664) + lmat(k, 664)
         mat(k, 666) = mat(k, 666) + lmat(k, 666)
         mat(k, 667) = mat(k, 667) + lmat(k, 667)
         mat(k, 673) = mat(k, 673) + lmat(k, 673)
         mat(k, 690) = mat(k, 690) + lmat(k, 690)
         mat(k, 691) = lmat(k, 691)
         mat(k, 694) = lmat(k, 694)
         mat(k, 696) = mat(k, 696) + lmat(k, 696)
         mat(k, 698) = lmat(k, 698)
         mat(k, 700) = mat(k, 700) + lmat(k, 700)
         mat(k, 701) = lmat(k, 701)
         mat(k, 702) = mat(k, 702) + lmat(k, 702)
         mat(k, 703) = mat(k, 703) + lmat(k, 703)
         mat(k, 704) = mat(k, 704) + lmat(k, 704)
         mat(k, 707) = mat(k, 707) + lmat(k, 707)
         mat(k, 721) = mat(k, 721) + lmat(k, 721)
         mat(k, 742) = mat(k, 742) + lmat(k, 742)
         mat(k, 764) = mat(k, 764) + lmat(k, 764)
         mat(k, 774) = mat(k, 774) + lmat(k, 774)
         mat(k, 776) = lmat(k, 776)
         mat(k, 777) = lmat(k, 777)
         mat(k, 781) = mat(k, 781) + lmat(k, 781)
         mat(k, 782) = mat(k, 782) + lmat(k, 782)
         mat(k, 783) = mat(k, 783) + lmat(k, 783)
         mat(k, 786) = mat(k, 786) + lmat(k, 786)
         mat(k, 787) = mat(k, 787) + lmat(k, 787)
         mat(k, 788) = lmat(k, 788)
         mat(k, 789) = mat(k, 789) + lmat(k, 789)
         mat(k, 792) = mat(k, 792) + lmat(k, 792)
         mat(k, 793) = lmat(k, 793)
         mat(k, 794) = lmat(k, 794)
         mat(k, 797) = lmat(k, 797)
         mat(k, 804) = mat(k, 804) + lmat(k, 804)
         mat(k, 820) = mat(k, 820) + lmat(k, 820)
         mat(k, 821) = lmat(k, 821)
         mat(k, 822) = mat(k, 822) + lmat(k, 822)
         mat(k, 824) = mat(k, 824) + lmat(k, 824)
         mat(k, 825) = lmat(k, 825)
         mat(k, 829) = mat(k, 829) + lmat(k, 829)
         mat(k, 833) = lmat(k, 833)
         mat(k, 834) = mat(k, 834) + lmat(k, 834)
         mat(k, 837) = lmat(k, 837)
         mat(k, 840) = mat(k, 840) + lmat(k, 840)
         mat(k, 841) = mat(k, 841) + lmat(k, 841)
         mat(k, 842) = mat(k, 842) + lmat(k, 842)
         mat(k, 843) = mat(k, 843) + lmat(k, 843)
         mat(k, 844) = mat(k, 844) + lmat(k, 844)
         mat(k, 845) = mat(k, 845) + lmat(k, 845)
         mat(k, 848) = mat(k, 848) + lmat(k, 848)
         mat(k, 851) = lmat(k, 851)
         mat(k, 852) = lmat(k, 852)
         mat(k, 853) = mat(k, 853) + lmat(k, 853)
         mat(k, 854) = lmat(k, 854)
         mat(k, 855) = lmat(k, 855)
         mat(k, 857) = lmat(k, 857)
         mat(k, 858) = lmat(k, 858)
         mat(k, 859) = lmat(k, 859)
         mat(k, 860) = mat(k, 860) + lmat(k, 860)
         mat(k, 863) = lmat(k, 863)
         mat(k, 864) = lmat(k, 864)
         mat(k, 866) = mat(k, 866) + lmat(k, 866)
         mat(k, 870) = mat(k, 870) + lmat(k, 870)
         mat(k, 872) = lmat(k, 872)
         mat(k, 873) = mat(k, 873) + lmat(k, 873)
         mat(k, 875) = lmat(k, 875)
         mat(k, 883) = mat(k, 883) + lmat(k, 883)
         mat(k, 905) = mat(k, 905) + lmat(k, 905)
         mat(k, 924) = mat(k, 924) + lmat(k, 924)
         mat(k, 940) = mat(k, 940) + lmat(k, 940)
         mat(k, 950) = lmat(k, 950)
         mat(k, 951) = mat(k, 951) + lmat(k, 951)
         mat(k, 955) = lmat(k, 955)
         mat(k, 956) = lmat(k, 956)
         mat(k, 962) = mat(k, 962) + lmat(k, 962)
         mat(k, 979) = mat(k, 979) + lmat(k, 979)
         mat(k, 999) = mat(k, 999) + lmat(k, 999)
         mat(k,1014) = mat(k,1014) + lmat(k,1014)
         mat(k,1015) = mat(k,1015) + lmat(k,1015)
         mat(k,1018) = mat(k,1018) + lmat(k,1018)
         mat(k,1019) = mat(k,1019) + lmat(k,1019)
         mat(k,1021) = mat(k,1021) + lmat(k,1021)
         mat(k,1024) = mat(k,1024) + lmat(k,1024)
         mat(k,1026) = mat(k,1026) + lmat(k,1026)
         mat(k,1027) = mat(k,1027) + lmat(k,1027)
         mat(k,1028) = mat(k,1028) + lmat(k,1028)
         mat(k,1031) = lmat(k,1031)
         mat(k,1036) = lmat(k,1036)
         mat(k,1037) = mat(k,1037) + lmat(k,1037)
         mat(k,1038) = mat(k,1038) + lmat(k,1038)
         mat(k,1048) = lmat(k,1048)
         mat(k,1063) = mat(k,1063) + lmat(k,1063)
         mat(k,1080) = lmat(k,1080)
         mat(k,1081) = mat(k,1081) + lmat(k,1081)
         mat(k,1085) = mat(k,1085) + lmat(k,1085)
         mat(k,1087) = mat(k,1087) + lmat(k,1087)
         mat(k,1091) = lmat(k,1091)
         mat(k,1108) = mat(k,1108) + lmat(k,1108)
         mat(k,1121) = lmat(k,1121)
         mat(k,1141) = mat(k,1141) + lmat(k,1141)
         mat(k,1146) = mat(k,1146) + lmat(k,1146)
         mat(k,1160) = mat(k,1160) + lmat(k,1160)
         mat(k,1191) = mat(k,1191) + lmat(k,1191)
         mat(k,1205) = mat(k,1205) + lmat(k,1205)
         mat(k,1218) = mat(k,1218) + lmat(k,1218)
         mat(k,1223) = mat(k,1223) + lmat(k,1223)
         mat(k,1225) = lmat(k,1225)
         mat(k,1231) = mat(k,1231) + lmat(k,1231)
         mat(k,1233) = mat(k,1233) + lmat(k,1233)
         mat(k,1250) = mat(k,1250) + lmat(k,1250)
         mat(k,1251) = mat(k,1251) + lmat(k,1251)
         mat(k,1256) = mat(k,1256) + lmat(k,1256)
         mat(k,1294) = mat(k,1294) + lmat(k,1294)
         mat(k,1344) = mat(k,1344) + lmat(k,1344)
         mat(k,1395) = mat(k,1395) + lmat(k,1395)
         mat(k,1408) = mat(k,1408) + lmat(k,1408)
         mat(k,1409) = mat(k,1409) + lmat(k,1409)
         mat(k,1411) = mat(k,1411) + lmat(k,1411)
         mat(k,1413) = mat(k,1413) + lmat(k,1413)
         mat(k,1414) = mat(k,1414) + lmat(k,1414)
         mat(k,1416) = mat(k,1416) + lmat(k,1416)
         mat(k,1418) = lmat(k,1418)
         mat(k,1419) = lmat(k,1419)
         mat(k,1420) = mat(k,1420) + lmat(k,1420)
         mat(k,1421) = mat(k,1421) + lmat(k,1421)
         mat(k,1422) = mat(k,1422) + lmat(k,1422)
         mat(k,1427) = lmat(k,1427)
         mat(k,1428) = lmat(k,1428)
         mat(k,1429) = lmat(k,1429)
         mat(k,1437) = lmat(k,1437)
         mat(k,1442) = lmat(k,1442)
         mat(k,1556) = mat(k,1556) + lmat(k,1556)
         mat(k,1560) = mat(k,1560) + lmat(k,1560)
         mat(k,1561) = mat(k,1561) + lmat(k,1561)
         mat(k,1563) = mat(k,1563) + lmat(k,1563)
         mat(k,1564) = mat(k,1564) + lmat(k,1564)
         mat(k,1573) = mat(k,1573) + lmat(k,1573)
         mat(k,1586) = mat(k,1586) + lmat(k,1586)
         mat(k,1590) = lmat(k,1590)
         mat(k,1593) = mat(k,1593) + lmat(k,1593)
         mat(k,1594) = mat(k,1594) + lmat(k,1594)
         mat(k,1595) = lmat(k,1595)
         mat(k,1598) = mat(k,1598) + lmat(k,1598)
         mat(k,1624) = mat(k,1624) + lmat(k,1624)
         mat(k,1625) = mat(k,1625) + lmat(k,1625)
         mat(k,1631) = mat(k,1631) + lmat(k,1631)
         mat(k,1679) = mat(k,1679) + lmat(k,1679)
         mat(k,1683) = mat(k,1683) + lmat(k,1683)
         mat(k,1688) = mat(k,1688) + lmat(k,1688)
         mat(k,1715) = mat(k,1715) + lmat(k,1715)
         mat(k,1721) = mat(k,1721) + lmat(k,1721)
         mat(k,1725) = mat(k,1725) + lmat(k,1725)
         mat(k,1728) = mat(k,1728) + lmat(k,1728)
         mat(k,1729) = mat(k,1729) + lmat(k,1729)
         mat(k,1771) = mat(k,1771) + lmat(k,1771)
         mat(k,1781) = mat(k,1781) + lmat(k,1781)
         mat(k,1782) = mat(k,1782) + lmat(k,1782)
         mat(k,1784) = mat(k,1784) + lmat(k,1784)
         mat(k,1785) = mat(k,1785) + lmat(k,1785)
         mat(k,1791) = mat(k,1791) + lmat(k,1791)
         mat(k,1792) = lmat(k,1792)
         mat(k,1795) = mat(k,1795) + lmat(k,1795)
         mat(k,1806) = mat(k,1806) + lmat(k,1806)
         mat(k,1817) = mat(k,1817) + lmat(k,1817)
         mat(k,1885) = mat(k,1885) + lmat(k,1885)
         mat(k,1886) = mat(k,1886) + lmat(k,1886)
         mat(k,1911) = mat(k,1911) + lmat(k,1911)
         mat(k,1916) = mat(k,1916) + lmat(k,1916)
         mat(k,1925) = mat(k,1925) + lmat(k,1925)
         mat(k,1940) = mat(k,1940) + lmat(k,1940)
         mat(k,1941) = mat(k,1941) + lmat(k,1941)
         mat(k,1948) = lmat(k,1948)
         mat(k,1952) = lmat(k,1952)
         mat(k,1956) = mat(k,1956) + lmat(k,1956)
         mat(k,1957) = mat(k,1957) + lmat(k,1957)
         mat(k,1965) = lmat(k,1965)
         mat(k,1967) = mat(k,1967) + lmat(k,1967)
         mat(k, 102) = 0._r8
         mat(k, 103) = 0._r8
         mat(k, 213) = 0._r8
         mat(k, 301) = 0._r8
         mat(k, 303) = 0._r8
         mat(k, 316) = 0._r8
         mat(k, 343) = 0._r8
         mat(k, 346) = 0._r8
         mat(k, 354) = 0._r8
         mat(k, 472) = 0._r8
         mat(k, 473) = 0._r8
         mat(k, 478) = 0._r8
         mat(k, 479) = 0._r8
         mat(k, 482) = 0._r8
         mat(k, 491) = 0._r8
         mat(k, 493) = 0._r8
         mat(k, 495) = 0._r8
         mat(k, 496) = 0._r8
         mat(k, 502) = 0._r8
         mat(k, 503) = 0._r8
         mat(k, 507) = 0._r8
         mat(k, 532) = 0._r8
         mat(k, 534) = 0._r8
         mat(k, 536) = 0._r8
         mat(k, 537) = 0._r8
         mat(k, 539) = 0._r8
         mat(k, 545) = 0._r8
         mat(k, 547) = 0._r8
         mat(k, 561) = 0._r8
         mat(k, 563) = 0._r8
         mat(k, 565) = 0._r8
         mat(k, 566) = 0._r8
         mat(k, 569) = 0._r8
         mat(k, 577) = 0._r8
         mat(k, 579) = 0._r8
         mat(k, 581) = 0._r8
         mat(k, 582) = 0._r8
         mat(k, 584) = 0._r8
         mat(k, 586) = 0._r8
         mat(k, 599) = 0._r8
         mat(k, 602) = 0._r8
         mat(k, 606) = 0._r8
         mat(k, 612) = 0._r8
         mat(k, 635) = 0._r8
         mat(k, 639) = 0._r8
         mat(k, 659) = 0._r8
         mat(k, 688) = 0._r8
         mat(k, 695) = 0._r8
         mat(k, 720) = 0._r8
         mat(k, 722) = 0._r8
         mat(k, 730) = 0._r8
         mat(k, 737) = 0._r8
         mat(k, 741) = 0._r8
         mat(k, 743) = 0._r8
         mat(k, 751) = 0._r8
         mat(k, 758) = 0._r8
         mat(k, 762) = 0._r8
         mat(k, 763) = 0._r8
         mat(k, 767) = 0._r8
         mat(k, 769) = 0._r8
         mat(k, 770) = 0._r8
         mat(k, 791) = 0._r8
         mat(k, 806) = 0._r8
         mat(k, 808) = 0._r8
         mat(k, 809) = 0._r8
         mat(k, 814) = 0._r8
         mat(k, 817) = 0._r8
         mat(k, 818) = 0._r8
         mat(k, 828) = 0._r8
         mat(k, 830) = 0._r8
         mat(k, 831) = 0._r8
         mat(k, 832) = 0._r8
         mat(k, 835) = 0._r8
         mat(k, 838) = 0._r8
         mat(k, 839) = 0._r8
         mat(k, 846) = 0._r8
         mat(k, 849) = 0._r8
         mat(k, 856) = 0._r8
         mat(k, 861) = 0._r8
         mat(k, 865) = 0._r8
         mat(k, 881) = 0._r8
         mat(k, 882) = 0._r8
         mat(k, 884) = 0._r8
         mat(k, 885) = 0._r8
         mat(k, 886) = 0._r8
         mat(k, 887) = 0._r8
         mat(k, 888) = 0._r8
         mat(k, 889) = 0._r8
         mat(k, 892) = 0._r8
         mat(k, 894) = 0._r8
         mat(k, 897) = 0._r8
         mat(k, 906) = 0._r8
         mat(k, 907) = 0._r8
         mat(k, 908) = 0._r8
         mat(k, 909) = 0._r8
         mat(k, 912) = 0._r8
         mat(k, 916) = 0._r8
         mat(k, 921) = 0._r8
         mat(k, 922) = 0._r8
         mat(k, 923) = 0._r8
         mat(k, 925) = 0._r8
         mat(k, 926) = 0._r8
         mat(k, 927) = 0._r8
         mat(k, 928) = 0._r8
         mat(k, 931) = 0._r8
         mat(k, 936) = 0._r8
         mat(k, 949) = 0._r8
         mat(k, 954) = 0._r8
         mat(k, 963) = 0._r8
         mat(k, 966) = 0._r8
         mat(k, 970) = 0._r8
         mat(k, 982) = 0._r8
         mat(k, 985) = 0._r8
         mat(k, 992) = 0._r8
         mat(k, 994) = 0._r8
         mat(k, 995) = 0._r8
         mat(k, 997) = 0._r8
         mat(k, 998) = 0._r8
         mat(k,1000) = 0._r8
         mat(k,1001) = 0._r8
         mat(k,1002) = 0._r8
         mat(k,1004) = 0._r8
         mat(k,1005) = 0._r8
         mat(k,1008) = 0._r8
         mat(k,1020) = 0._r8
         mat(k,1030) = 0._r8
         mat(k,1041) = 0._r8
         mat(k,1050) = 0._r8
         mat(k,1054) = 0._r8
         mat(k,1055) = 0._r8
         mat(k,1056) = 0._r8
         mat(k,1057) = 0._r8
         mat(k,1058) = 0._r8
         mat(k,1059) = 0._r8
         mat(k,1060) = 0._r8
         mat(k,1062) = 0._r8
         mat(k,1065) = 0._r8
         mat(k,1066) = 0._r8
         mat(k,1068) = 0._r8
         mat(k,1069) = 0._r8
         mat(k,1072) = 0._r8
         mat(k,1073) = 0._r8
         mat(k,1078) = 0._r8
         mat(k,1082) = 0._r8
         mat(k,1088) = 0._r8
         mat(k,1089) = 0._r8
         mat(k,1094) = 0._r8
         mat(k,1095) = 0._r8
         mat(k,1097) = 0._r8
         mat(k,1098) = 0._r8
         mat(k,1105) = 0._r8
         mat(k,1106) = 0._r8
         mat(k,1109) = 0._r8
         mat(k,1111) = 0._r8
         mat(k,1112) = 0._r8
         mat(k,1115) = 0._r8
         mat(k,1120) = 0._r8
         mat(k,1124) = 0._r8
         mat(k,1127) = 0._r8
         mat(k,1129) = 0._r8
         mat(k,1131) = 0._r8
         mat(k,1132) = 0._r8
         mat(k,1134) = 0._r8
         mat(k,1135) = 0._r8
         mat(k,1136) = 0._r8
         mat(k,1137) = 0._r8
         mat(k,1139) = 0._r8
         mat(k,1140) = 0._r8
         mat(k,1142) = 0._r8
         mat(k,1144) = 0._r8
         mat(k,1145) = 0._r8
         mat(k,1148) = 0._r8
         mat(k,1149) = 0._r8
         mat(k,1154) = 0._r8
         mat(k,1158) = 0._r8
         mat(k,1159) = 0._r8
         mat(k,1162) = 0._r8
         mat(k,1163) = 0._r8
         mat(k,1172) = 0._r8
         mat(k,1192) = 0._r8
         mat(k,1193) = 0._r8
         mat(k,1199) = 0._r8
         mat(k,1202) = 0._r8
         mat(k,1206) = 0._r8
         mat(k,1208) = 0._r8
         mat(k,1209) = 0._r8
         mat(k,1212) = 0._r8
         mat(k,1214) = 0._r8
         mat(k,1217) = 0._r8
         mat(k,1219) = 0._r8
         mat(k,1220) = 0._r8
         mat(k,1221) = 0._r8
         mat(k,1222) = 0._r8
         mat(k,1224) = 0._r8
         mat(k,1227) = 0._r8
         mat(k,1230) = 0._r8
         mat(k,1232) = 0._r8
         mat(k,1234) = 0._r8
         mat(k,1235) = 0._r8
         mat(k,1237) = 0._r8
         mat(k,1239) = 0._r8
         mat(k,1240) = 0._r8
         mat(k,1248) = 0._r8
         mat(k,1252) = 0._r8
         mat(k,1253) = 0._r8
         mat(k,1258) = 0._r8
         mat(k,1259) = 0._r8
         mat(k,1260) = 0._r8
         mat(k,1261) = 0._r8
         mat(k,1263) = 0._r8
         mat(k,1268) = 0._r8
         mat(k,1285) = 0._r8
         mat(k,1286) = 0._r8
         mat(k,1314) = 0._r8
         mat(k,1318) = 0._r8
         mat(k,1319) = 0._r8
         mat(k,1320) = 0._r8
         mat(k,1321) = 0._r8
         mat(k,1323) = 0._r8
         mat(k,1332) = 0._r8
         mat(k,1335) = 0._r8
         mat(k,1341) = 0._r8
         mat(k,1346) = 0._r8
         mat(k,1367) = 0._r8
         mat(k,1391) = 0._r8
         mat(k,1392) = 0._r8
         mat(k,1393) = 0._r8
         mat(k,1396) = 0._r8
         mat(k,1397) = 0._r8
         mat(k,1400) = 0._r8
         mat(k,1402) = 0._r8
         mat(k,1405) = 0._r8
         mat(k,1406) = 0._r8
         mat(k,1407) = 0._r8
         mat(k,1415) = 0._r8
         mat(k,1425) = 0._r8
         mat(k,1426) = 0._r8
         mat(k,1480) = 0._r8
         mat(k,1497) = 0._r8
         mat(k,1509) = 0._r8
         mat(k,1512) = 0._r8
         mat(k,1514) = 0._r8
         mat(k,1525) = 0._r8
         mat(k,1546) = 0._r8
         mat(k,1562) = 0._r8
         mat(k,1579) = 0._r8
         mat(k,1580) = 0._r8
         mat(k,1583) = 0._r8
         mat(k,1585) = 0._r8
         mat(k,1587) = 0._r8
         mat(k,1589) = 0._r8
         mat(k,1591) = 0._r8
         mat(k,1596) = 0._r8
         mat(k,1601) = 0._r8
         mat(k,1604) = 0._r8
         mat(k,1605) = 0._r8
         mat(k,1606) = 0._r8
         mat(k,1607) = 0._r8
         mat(k,1617) = 0._r8
         mat(k,1618) = 0._r8
         mat(k,1622) = 0._r8
         mat(k,1626) = 0._r8
         mat(k,1628) = 0._r8
         mat(k,1633) = 0._r8
         mat(k,1643) = 0._r8
         mat(k,1647) = 0._r8
         mat(k,1650) = 0._r8
         mat(k,1654) = 0._r8
         mat(k,1657) = 0._r8
         mat(k,1658) = 0._r8
         mat(k,1659) = 0._r8
         mat(k,1660) = 0._r8
         mat(k,1663) = 0._r8
         mat(k,1667) = 0._r8
         mat(k,1669) = 0._r8
         mat(k,1670) = 0._r8
         mat(k,1671) = 0._r8
         mat(k,1674) = 0._r8
         mat(k,1676) = 0._r8
         mat(k,1690) = 0._r8
         mat(k,1700) = 0._r8
         mat(k,1703) = 0._r8
         mat(k,1705) = 0._r8
         mat(k,1708) = 0._r8
         mat(k,1709) = 0._r8
         mat(k,1710) = 0._r8
         mat(k,1714) = 0._r8
         mat(k,1716) = 0._r8
         mat(k,1717) = 0._r8
         mat(k,1719) = 0._r8
         mat(k,1720) = 0._r8
         mat(k,1722) = 0._r8
         mat(k,1727) = 0._r8
         mat(k,1731) = 0._r8
         mat(k,1737) = 0._r8
         mat(k,1744) = 0._r8
         mat(k,1750) = 0._r8
         mat(k,1751) = 0._r8
         mat(k,1754) = 0._r8
         mat(k,1757) = 0._r8
         mat(k,1763) = 0._r8
         mat(k,1770) = 0._r8
         mat(k,1772) = 0._r8
         mat(k,1773) = 0._r8
         mat(k,1775) = 0._r8
         mat(k,1776) = 0._r8
         mat(k,1778) = 0._r8
         mat(k,1779) = 0._r8
         mat(k,1780) = 0._r8
         mat(k,1786) = 0._r8
         mat(k,1787) = 0._r8
         mat(k,1789) = 0._r8
         mat(k,1798) = 0._r8
         mat(k,1799) = 0._r8
         mat(k,1802) = 0._r8
         mat(k,1803) = 0._r8
         mat(k,1804) = 0._r8
         mat(k,1807) = 0._r8
         mat(k,1809) = 0._r8
         mat(k,1844) = 0._r8
         mat(k,1872) = 0._r8
         mat(k,1873) = 0._r8
         mat(k,1874) = 0._r8
         mat(k,1877) = 0._r8
         mat(k,1888) = 0._r8
         mat(k,1890) = 0._r8
         mat(k,1896) = 0._r8
         mat(k,1902) = 0._r8
         mat(k,1906) = 0._r8
         mat(k,1907) = 0._r8
         mat(k,1918) = 0._r8
         mat(k,1926) = 0._r8
         mat(k,1927) = 0._r8
         mat(k,1928) = 0._r8
         mat(k,1930) = 0._r8
         mat(k,1931) = 0._r8
         mat(k,1935) = 0._r8
         mat(k,1937) = 0._r8
         mat(k,1938) = 0._r8
         mat(k,1942) = 0._r8
         mat(k,1947) = 0._r8
         mat(k,1949) = 0._r8
         mat(k,1950) = 0._r8
         mat(k,1951) = 0._r8
         mat(k,1953) = 0._r8
         mat(k,1954) = 0._r8
         mat(k,1955) = 0._r8
         mat(k,1958) = 0._r8
         mat(k,1959) = 0._r8
         mat(k,1960) = 0._r8
         mat(k,1961) = 0._r8
         mat(k,1962) = 0._r8
         mat(k,1963) = 0._r8
         mat(k,1964) = 0._r8
         mat(k,1966) = 0._r8
         mat(k, 1) = mat(k, 1) - dti(k)
         mat(k, 2) = mat(k, 2) - dti(k)
         mat(k, 3) = mat(k, 3) - dti(k)
         mat(k, 4) = mat(k, 4) - dti(k)
         mat(k, 5) = mat(k, 5) - dti(k)
         mat(k, 6) = mat(k, 6) - dti(k)
         mat(k, 7) = mat(k, 7) - dti(k)
         mat(k, 8) = mat(k, 8) - dti(k)
         mat(k, 9) = mat(k, 9) - dti(k)
         mat(k, 10) = mat(k, 10) - dti(k)
         mat(k, 11) = mat(k, 11) - dti(k)
         mat(k, 12) = mat(k, 12) - dti(k)
         mat(k, 13) = mat(k, 13) - dti(k)
         mat(k, 14) = mat(k, 14) - dti(k)
         mat(k, 15) = mat(k, 15) - dti(k)
         mat(k, 16) = mat(k, 16) - dti(k)
         mat(k, 17) = mat(k, 17) - dti(k)
         mat(k, 18) = mat(k, 18) - dti(k)
         mat(k, 19) = mat(k, 19) - dti(k)
         mat(k, 20) = mat(k, 20) - dti(k)
         mat(k, 21) = mat(k, 21) - dti(k)
         mat(k, 22) = mat(k, 22) - dti(k)
         mat(k, 23) = mat(k, 23) - dti(k)
         mat(k, 24) = mat(k, 24) - dti(k)
         mat(k, 25) = mat(k, 25) - dti(k)
         mat(k, 28) = mat(k, 28) - dti(k)
         mat(k, 31) = mat(k, 31) - dti(k)
         mat(k, 34) = mat(k, 34) - dti(k)
         mat(k, 38) = mat(k, 38) - dti(k)
         mat(k, 41) = mat(k, 41) - dti(k)
         mat(k, 44) = mat(k, 44) - dti(k)
         mat(k, 47) = mat(k, 47) - dti(k)
         mat(k, 50) = mat(k, 50) - dti(k)
         mat(k, 53) = mat(k, 53) - dti(k)
         mat(k, 55) = mat(k, 55) - dti(k)
         mat(k, 61) = mat(k, 61) - dti(k)
         mat(k, 65) = mat(k, 65) - dti(k)
         mat(k, 70) = mat(k, 70) - dti(k)
         mat(k, 74) = mat(k, 74) - dti(k)
         mat(k, 78) = mat(k, 78) - dti(k)
         mat(k, 85) = mat(k, 85) - dti(k)
         mat(k, 90) = mat(k, 90) - dti(k)
         mat(k, 94) = mat(k, 94) - dti(k)
         mat(k, 98) = mat(k, 98) - dti(k)
         mat(k, 106) = mat(k, 106) - dti(k)
         mat(k, 111) = mat(k, 111) - dti(k)
         mat(k, 114) = mat(k, 114) - dti(k)
         mat(k, 117) = mat(k, 117) - dti(k)
         mat(k, 120) = mat(k, 120) - dti(k)
         mat(k, 125) = mat(k, 125) - dti(k)
         mat(k, 128) = mat(k, 128) - dti(k)
         mat(k, 131) = mat(k, 131) - dti(k)
         mat(k, 136) = mat(k, 136) - dti(k)
         mat(k, 140) = mat(k, 140) - dti(k)
         mat(k, 144) = mat(k, 144) - dti(k)
         mat(k, 148) = mat(k, 148) - dti(k)
         mat(k, 156) = mat(k, 156) - dti(k)
         mat(k, 162) = mat(k, 162) - dti(k)
         mat(k, 165) = mat(k, 165) - dti(k)
         mat(k, 171) = mat(k, 171) - dti(k)
         mat(k, 177) = mat(k, 177) - dti(k)
         mat(k, 182) = mat(k, 182) - dti(k)
         mat(k, 187) = mat(k, 187) - dti(k)
         mat(k, 190) = mat(k, 190) - dti(k)
         mat(k, 195) = mat(k, 195) - dti(k)
         mat(k, 200) = mat(k, 200) - dti(k)
         mat(k, 205) = mat(k, 205) - dti(k)
         mat(k, 211) = mat(k, 211) - dti(k)
         mat(k, 216) = mat(k, 216) - dti(k)
         mat(k, 224) = mat(k, 224) - dti(k)
         mat(k, 232) = mat(k, 232) - dti(k)
         mat(k, 238) = mat(k, 238) - dti(k)
         mat(k, 244) = mat(k, 244) - dti(k)
         mat(k, 250) = mat(k, 250) - dti(k)
         mat(k, 256) = mat(k, 256) - dti(k)
         mat(k, 262) = mat(k, 262) - dti(k)
         mat(k, 268) = mat(k, 268) - dti(k)
         mat(k, 274) = mat(k, 274) - dti(k)
         mat(k, 280) = mat(k, 280) - dti(k)
         mat(k, 286) = mat(k, 286) - dti(k)
         mat(k, 292) = mat(k, 292) - dti(k)
         mat(k, 300) = mat(k, 300) - dti(k)
         mat(k, 306) = mat(k, 306) - dti(k)
         mat(k, 313) = mat(k, 313) - dti(k)
         mat(k, 319) = mat(k, 319) - dti(k)
         mat(k, 322) = mat(k, 322) - dti(k)
         mat(k, 329) = mat(k, 329) - dti(k)
         mat(k, 333) = mat(k, 333) - dti(k)
         mat(k, 342) = mat(k, 342) - dti(k)
         mat(k, 350) = mat(k, 350) - dti(k)
         mat(k, 357) = mat(k, 357) - dti(k)
         mat(k, 363) = mat(k, 363) - dti(k)
         mat(k, 368) = mat(k, 368) - dti(k)
         mat(k, 375) = mat(k, 375) - dti(k)
         mat(k, 381) = mat(k, 381) - dti(k)
         mat(k, 389) = mat(k, 389) - dti(k)
         mat(k, 397) = mat(k, 397) - dti(k)
         mat(k, 401) = mat(k, 401) - dti(k)
         mat(k, 409) = mat(k, 409) - dti(k)
         mat(k, 417) = mat(k, 417) - dti(k)
         mat(k, 425) = mat(k, 425) - dti(k)
         mat(k, 433) = mat(k, 433) - dti(k)
         mat(k, 440) = mat(k, 440) - dti(k)
         mat(k, 444) = mat(k, 444) - dti(k)
         mat(k, 453) = mat(k, 453) - dti(k)
         mat(k, 462) = mat(k, 462) - dti(k)
         mat(k, 470) = mat(k, 470) - dti(k)
         mat(k, 477) = mat(k, 477) - dti(k)
         mat(k, 490) = mat(k, 490) - dti(k)
         mat(k, 501) = mat(k, 501) - dti(k)
         mat(k, 512) = mat(k, 512) - dti(k)
         mat(k, 520) = mat(k, 520) - dti(k)
         mat(k, 531) = mat(k, 531) - dti(k)
         mat(k, 544) = mat(k, 544) - dti(k)
         mat(k, 551) = mat(k, 551) - dti(k)
         mat(k, 562) = mat(k, 562) - dti(k)
         mat(k, 578) = mat(k, 578) - dti(k)
         mat(k, 589) = mat(k, 589) - dti(k)
         mat(k, 598) = mat(k, 598) - dti(k)
         mat(k, 608) = mat(k, 608) - dti(k)
         mat(k, 617) = mat(k, 617) - dti(k)
         mat(k, 625) = mat(k, 625) - dti(k)
         mat(k, 630) = mat(k, 630) - dti(k)
         mat(k, 640) = mat(k, 640) - dti(k)
         mat(k, 644) = mat(k, 644) - dti(k)
         mat(k, 655) = mat(k, 655) - dti(k)
         mat(k, 664) = mat(k, 664) - dti(k)
         mat(k, 673) = mat(k, 673) - dti(k)
         mat(k, 690) = mat(k, 690) - dti(k)
         mat(k, 696) = mat(k, 696) - dti(k)
         mat(k, 702) = mat(k, 702) - dti(k)
         mat(k, 707) = mat(k, 707) - dti(k)
         mat(k, 721) = mat(k, 721) - dti(k)
         mat(k, 742) = mat(k, 742) - dti(k)
         mat(k, 764) = mat(k, 764) - dti(k)
         mat(k, 774) = mat(k, 774) - dti(k)
         mat(k, 782) = mat(k, 782) - dti(k)
         mat(k, 792) = mat(k, 792) - dti(k)
         mat(k, 804) = mat(k, 804) - dti(k)
         mat(k, 820) = mat(k, 820) - dti(k)
         mat(k, 829) = mat(k, 829) - dti(k)
         mat(k, 841) = mat(k, 841) - dti(k)
         mat(k, 853) = mat(k, 853) - dti(k)
         mat(k, 866) = mat(k, 866) - dti(k)
         mat(k, 870) = mat(k, 870) - dti(k)
         mat(k, 883) = mat(k, 883) - dti(k)
         mat(k, 905) = mat(k, 905) - dti(k)
         mat(k, 924) = mat(k, 924) - dti(k)
         mat(k, 940) = mat(k, 940) - dti(k)
         mat(k, 951) = mat(k, 951) - dti(k)
         mat(k, 962) = mat(k, 962) - dti(k)
         mat(k, 979) = mat(k, 979) - dti(k)
         mat(k, 999) = mat(k, 999) - dti(k)
         mat(k,1015) = mat(k,1015) - dti(k)
         mat(k,1027) = mat(k,1027) - dti(k)
         mat(k,1038) = mat(k,1038) - dti(k)
         mat(k,1063) = mat(k,1063) - dti(k)
         mat(k,1085) = mat(k,1085) - dti(k)
         mat(k,1108) = mat(k,1108) - dti(k)
         mat(k,1141) = mat(k,1141) - dti(k)
         mat(k,1160) = mat(k,1160) - dti(k)
         mat(k,1191) = mat(k,1191) - dti(k)
         mat(k,1205) = mat(k,1205) - dti(k)
         mat(k,1218) = mat(k,1218) - dti(k)
         mat(k,1231) = mat(k,1231) - dti(k)
         mat(k,1251) = mat(k,1251) - dti(k)
         mat(k,1344) = mat(k,1344) - dti(k)
         mat(k,1395) = mat(k,1395) - dti(k)
         mat(k,1420) = mat(k,1420) - dti(k)
         mat(k,1563) = mat(k,1563) - dti(k)
         mat(k,1598) = mat(k,1598) - dti(k)
         mat(k,1625) = mat(k,1625) - dti(k)
         mat(k,1683) = mat(k,1683) - dti(k)
         mat(k,1725) = mat(k,1725) - dti(k)
         mat(k,1782) = mat(k,1782) - dti(k)
         mat(k,1806) = mat(k,1806) - dti(k)
         mat(k,1885) = mat(k,1885) - dti(k)
         mat(k,1916) = mat(k,1916) - dti(k)
         mat(k,1941) = mat(k,1941) - dti(k)
         mat(k,1967) = mat(k,1967) - dti(k)
      end do
      end subroutine nlnmat_finit
      subroutine nlnmat( avec_len, mat, y, rxt, lmat, dti )
      use chem_mods, only : gas_pcnst, rxntot, nzcnt
      implicit none
!----------------------------------------------
! ... dummy arguments
!----------------------------------------------
      integer, intent(in) :: avec_len
      real(r8), intent(in) :: dti(veclen)
      real(r8), intent(in) :: lmat(veclen,nzcnt)
      real(r8), intent(in) :: y(veclen,gas_pcnst)
      real(r8), intent(in) :: rxt(veclen,rxntot)
      real(r8), intent(inout) :: mat(veclen,nzcnt)
      call nlnmat01( avec_len, mat, y, rxt )
      call nlnmat02( avec_len, mat, y, rxt )
      call nlnmat03( avec_len, mat, y, rxt )
      call nlnmat04( avec_len, mat, y, rxt )
      call nlnmat05( avec_len, mat, y, rxt )
      call nlnmat06( avec_len, mat, y, rxt )
      call nlnmat07( avec_len, mat, y, rxt )
      call nlnmat08( avec_len, mat, y, rxt )
      call nlnmat09( avec_len, mat, y, rxt )
      call nlnmat_finit( avec_len, mat, lmat, dti )
      end subroutine nlnmat
      end module mo_nln_matrix
