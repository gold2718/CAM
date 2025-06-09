!================================================================================================
! This is the 'sec_dust' chemistry module.
! It was copied from the 'none' chemistry module.
! Most of the routines return without doing anything.
!================================================================================================

module chemistry
  use shr_kind_mod,        only: r8 => shr_kind_r8
  use physics_types,       only: physics_state, physics_ptend
  use ppgrid,              only: begchunk, endchunk, pcols
  use mo_gas_phase_chemdr, only : map2chm
  use spmd_utils,       only : masterproc
  use cam_logfile,      only : iulog


  implicit none
  private
  save
  !
  ! Public interfaces
  !
  public :: chem_is                        ! identify which chemistry is being used
  public :: chem_register                  ! register consituents
  public :: chem_is_active                 ! returns true if this package is active (ghg_chem=.true.)
  public :: chem_implements_cnst           ! returns true if consituent is implemented by this package
  public :: chem_init_cnst                 ! initialize mixing ratios if not read from initial file
  public :: chem_init                      ! initialize (history) variables
  public :: chem_timestep_init             ! time interpolate chemical loss frequencies
  public :: chem_timestep_tend             ! interface to tendency computation
  public :: chem_final
  public :: chem_write_restart
  public :: chem_read_restart
  public :: chem_init_restart
  public :: chem_readnl                    ! read chem namelist
  public :: chem_reset_fluxes
  public :: chem_emissions

  interface chem_write_restart
     module procedure chem_write_restart_bin
     module procedure chem_write_restart_pio
  end interface
  interface chem_read_restart
     module procedure chem_read_restart_bin
     module procedure chem_read_restart_pio
  end interface

  ! species indices
     integer :: h2o_ndx

!================================================================================================
contains
!================================================================================================

  logical function chem_is (name)

    character(len=*), intent(in) :: name

    chem_is = .false.
    if (name == 'none' ) then
       chem_is = .true.
    end if

  end function chem_is

!================================================================================================

  subroutine chem_register

!-----------------------------------------------------------------------
!
! Purpose: register advected constituents for parameterized greenhouse gas chemistry
!
!-----------------------------------------------------------------------

    use aero_model,     only : aero_model_register
    use constituents,   only : cnst_add, cnst_name
    use mo_sim_dat,     only : set_sim_dat
    use mo_tracname,    only : solsym
    use mo_chem_utls,   only : get_spc_ndx, get_inv_ndx
    use chem_mods,      only : adv_mass, gas_pcnst

    implicit none

!-----------------------------------------------------------------------
! Local variables - check how and where these are initialized!!
!-----------------------------------------------------------------------
    integer               :: m, n            ! Tracer index
    real(r8), parameter   :: cptmp = 666._r8 ! specific heat at cnst prs (from mozart/chemistry.F90)
    real(r8)              :: qmin            ! min value
    logical               :: cam_outfld
    character(len=128)    :: lng_name        ! variable long name


!-----------------------------------------------------------------------
! Set the simulation chemistry variables
!-----------------------------------------------------------------------
! - currently no chemistry - only aerosol
    call set_sim_dat ! get arrays/vars from mo_sim_dat


    h2o_ndx   = get_spc_ndx('H2O')

!--------------------------------------------------------------
! Set names of diffused variable tendencies and declare them as history variables
!-----------------------------------------------------------------------
    do m = 1, gas_pcnst !
      lng_name = trim( solsym(m) )

      qmin = 1.e-36_r8
      if ( m == h2o_ndx ) then
        map2chm(1) = m
        cycle
      endif

      call cnst_add( solsym(m), adv_mass(m), cptmp, qmin, n, cam_outfld=cam_outfld, &
                         longname=trim(lng_name) )
    end do
   ! for prescribed aerosols
    call aero_model_register()
  end subroutine chem_register

!================================================================================================

  subroutine chem_readnl(nlfile)

    ! args

    character(len=*), intent(in) :: nlfile  ! filepath for file containing namelist input


  end subroutine chem_readnl

!================================================================================================

  function chem_is_active()
    !-----------------------------------------------------------------------
    logical :: chem_is_active
    !-----------------------------------------------------------------------
    chem_is_active = .false.
  end function chem_is_active

!================================================================================================

  function chem_implements_cnst(name)
    !-----------------------------------------------------------------------
    !
    ! Purpose: return true if specified constituent is implemented by this package
    !
    ! Author: B. Eaton
    !
    !-----------------------------------------------------------------------
    implicit none
    !-----------------------------Arguments---------------------------------

    character(len=*), intent(in) :: name   ! constituent name
    logical :: chem_implements_cnst        ! return value

    chem_implements_cnst = .false.

  end function chem_implements_cnst

!===============================================================================

  subroutine chem_init(phys_state, pbuf2d)
    !-----------------------------------------------------------------------
    !
    ! Purpose: initialize parameterized greenhouse gas chemistry
    !          (declare history variables)
    !
    !-----------------------------------------------------------------------
    use physics_buffer, only : physics_buffer_desc, pbuf_get_index, pbuf_set_field
    use aero_model,     only : aero_model_init

    type(physics_state), intent(in):: phys_state(begchunk:endchunk)
    type(physics_buffer_desc), pointer :: pbuf2d(:,:)

   ! for prescribed aerosols
    call aero_model_init(pbuf2d)

  end subroutine chem_init

!===============================================================================

  subroutine chem_timestep_init(phys_state, pbuf2d)
    use physics_buffer, only : physics_buffer_desc
    use time_manager, only: get_curr_date, get_perp_date, get_curr_calday, &
         is_perpetual
    type(physics_state), intent(in):: phys_state(begchunk:endchunk)
    type(physics_buffer_desc), pointer :: pbuf2d(:,:)



  end subroutine chem_timestep_init

!===============================================================================

  subroutine chem_timestep_tend( state, ptend, cam_in, cam_out, dt, pbuf, fh2o)
    use physics_buffer,           only: physics_buffer_desc
    use cam_history,      only: outfld
    use camsrfexch,       only: cam_in_t, cam_out_t
    !-----------------------------------------------------------------------
    !
    ! Arguments:
    !
    real(r8),            intent(in)    :: dt          ! time step
    type(physics_state), intent(in)    :: state       ! Physics state variables
    type(physics_ptend), intent(out)   :: ptend       ! indivdual parameterization tendencies
    type(cam_in_t),      intent(inout) :: cam_in
    type(cam_out_t),     intent(in)    :: cam_out
    type(physics_buffer_desc), pointer :: pbuf(:)
    real(r8), optional,  intent(out)   :: fh2o(pcols) ! h2o flux to balance source from chemistry

    return
  end subroutine chem_timestep_tend

!===============================================================================

  subroutine chem_init_cnst(name, latvals, lonvals, mask, q)

    character(len=*), intent(in)  :: name       ! constituent name
    real(r8),         intent(in)  :: latvals(:) ! lat in degrees (ncol)
    real(r8),         intent(in)  :: lonvals(:) ! lon in degrees (ncol)
    logical,          intent(in)  :: mask(:)    ! Only initialize where .true.
    real(r8),         intent(out) :: q(:,:)     ! kg tracer/kg dry air (gcol, plev

    return
  end subroutine chem_init_cnst

!===============================================================================
  subroutine chem_final
    return
  end subroutine chem_final
!===============================================================================
  subroutine chem_write_restart_bin( nrg )
    implicit none
    integer,intent(in) :: nrg     ! Unit number
    return
  end subroutine chem_write_restart_bin
!===============================================================================
  subroutine chem_read_restart_bin( nrg )
    implicit none
    integer,intent(in) :: nrg     ! Unit number
    return
  end subroutine chem_read_restart_bin
!===============================================================================
  subroutine chem_write_restart_pio( File )
    use pio, only : file_desc_t
    type(file_desc_t) :: File
    return
  end subroutine chem_write_restart_pio
!===============================================================================
  subroutine chem_read_restart_pio( File )
    use pio, only : file_desc_t
    type(file_desc_t) :: File
    return
  end subroutine chem_read_restart_pio
!===============================================================================
  subroutine chem_init_restart(File)
    use pio, only : file_desc_t
    type(file_desc_t) :: File
    return
  end subroutine chem_init_restart
!================================================================================
  subroutine chem_reset_fluxes( fptr, cam_in )
    use camsrfexch,          only : cam_in_t

    real(r8), pointer             :: fptr(:,:)        ! pointer into    array data
    type(cam_in_t), intent(inout) :: cam_in(begchunk:endchunk)

  end subroutine chem_reset_fluxes
!================================================================================
  subroutine chem_emissions( state, cam_in, pbuf )
    use camsrfexch,       only: cam_in_t
    use physics_buffer,   only: physics_buffer_desc

    ! Arguments:

    type(physics_state),    intent(in)    :: state   ! Physics state variables
    type(cam_in_t),         intent(inout) :: cam_in  ! import state
    type(physics_buffer_desc), pointer    :: pbuf(:) ! Physics buffer in chunk, for HEMCO

  end subroutine chem_emissions
end module chemistry
