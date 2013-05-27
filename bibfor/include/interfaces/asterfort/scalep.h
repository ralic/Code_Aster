        interface
          subroutine scalep(spectr,noma,base,nuor,nbm,imodi,nbmr,&
     &nbexcp,ltable,iaxe,scal)
            integer :: nbexcp
            integer :: nbmr
            integer :: nbm
            character(len=19) :: spectr
            character(len=8) :: noma
            character(len=19) :: base
            integer :: nuor(nbm)
            integer :: imodi
            logical :: ltable
            integer :: iaxe
            real(kind=8) :: scal(nbexcp,nbmr)
          end subroutine scalep
        end interface
