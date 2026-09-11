
module constituent_burden

!-----------------------------------------------------------------------------------------
! Purpose: subroutines to generate constituent burden history variables
!
! Revision history:
! 2005-12-21  K. Lindsay       Original version
!-----------------------------------------------------------------------------------------

  use constituents, only: pcnst
  use cam_history_support,  only: fieldname_len
  use co2_cycle, only: ncnst, c_i, co2_transport

  implicit none

! Public interfaces

  public constituent_burden_init
  public constituent_burden_comp

  private

  character(len=fieldname_len) :: burdennam(pcnst)      ! name of burden history variables
  character(len=fieldname_len) :: burdennam_inst(ncnst) ! name of instantaneous burden history variables

!=========================================================================================

contains

!=========================================================================================

subroutine constituent_burden_init

  use cam_history,   only: addfld, horiz_only
  use constituents,  only: cnst_name

  integer :: mind

  do mind = 2, pcnst
    burdennam(m) = 'TM'//trim(cnst_name(mind))
    call addfld(burdennam(mind), horiz_only, 'A', 'kg/m2', &
         trim(cnst_name(mind)) // ' column burden')
  end do
  if (co2_transport()) then
     do mind = 1, ncnst
        burdennam_inst(mind) = 'TM'//trim(cnst_name(c_i(mind)))//'_INST'
        call addfld(burdennam_inst(mind), horiz_only, 'A', 'kg/m2', &
             trim(cnst_name(c_i(mind))) // ' column burden for instantaneous output')
     end do
  else
     burdennam_inst(1) = 'TMCO2_INST'
     call addfld(burdennam_inst(1), horiz_only, 'A', 'kg/m2', &
          'CO2 column burden for instantaneous output')
     burdennam_inst(2:) = ''
  end if

end subroutine constituent_burden_init

!=========================================================================================

subroutine constituent_burden_comp(state)

  use physics_types, only: physics_state
  use shr_kind_mod,  only: r8 => shr_kind_r8
  use constituents,  only: cnst_type, cnst_get_ind
  use ppgrid,        only: pcols
  use physconst,     only: rga
  use cam_history,   only: outfld, hist_fld_active
  use chem_surfvals, only: chem_surfvals_get

!-----------------------------------------------------------------------
!
! Arguments
!
   type(physics_state), intent(inout) :: state
!
!---------------------------Local workspace-----------------------------

  real(r8) :: ftem(pcols)      ! temporary workspace

  integer :: mind, lchnk, ncol, cind

  lchnk = state%lchnk
  ncol  = state%ncol

  do mind = 2, pcnst
     if (.not. hist_fld_active(burdennam(mind))) cycle
     if (cnst_type(mind) .eq. 'dry') then
        ftem(:ncol) = sum(state%q(:ncol,:,mind) * state%pdeldry(:ncol,:), dim=2) * rga
     else
        ftem(:ncol) = sum(state%q(:ncol,:,mind) * state%pdel(:ncol,:), dim=2) * rga
     end if
     call outfld (burdennam(mind), ftem, pcols, lchnk)
  end do
  ! Compute special instantaneous values
  if (co2_transport()) then
     do mind = 1, ncnst
        if (.not. hist_fld_active(burdennam_inst(mind))) cycle
        cind = c_i(mind)
        if (cnst_type(cind) .eq. 'dry') then
           ftem(:ncol) = sum(state%q(:ncol,:,cind) * state%pdeldry(:ncol,:), dim=2) * rga
        else
           ftem(:ncol) = sum(state%q(:ncol,:,cind) * state%pdel(:ncol,:), dim=2) * rga
        end if
        call outfld(burdennam(mind), ftem(:ncol), ncol, lchnk)
     end do
  else if (hist_fld_active(burdennam_inst(1))) then
        call cnst_get_ind('CO2', cind, abort=.false.)
        if (cind > 0) then
           if (cnst_type(cind) .eq. 'dry') then
              ftem(:ncol) = sum(state%q(:ncol,:,cind) * state%pdeldry(:ncol,:), dim=2) * rga
           else
              ftem(:ncol) = sum(state%q(:ncol,:,cind) * state%pdel(:ncol,:), dim=2) * rga
           end if
        else
           ! There is no CO2 tracer, compute from co2mmr
           ftem(:ncol) = chem_surfvals_get('CO2MMR', lchnk, ncol) * sum(state%pdeldry(:ncol,:), dim=2) * rga
        end if
        call outfld(burdennam(1), ftem(:ncol), ncol, lchnk)
  end if

end subroutine constituent_burden_comp

!=========================================================================================

end module constituent_burden
