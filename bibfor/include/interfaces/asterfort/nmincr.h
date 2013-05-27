        interface
          subroutine nmincr(sddyna,ddincr,coedep,coevit,coeacc,dddepl,&
     &ddvite,ddacce)
            character(len=19) :: sddyna
            character(len=19) :: ddincr
            real(kind=8) :: coedep
            real(kind=8) :: coevit
            real(kind=8) :: coeacc
            character(len=19) :: dddepl
            character(len=19) :: ddvite
            character(len=19) :: ddacce
          end subroutine nmincr
        end interface
