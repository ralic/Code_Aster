        interface
          subroutine defttr(np1,np4,nbm,npf,nttr,ntrans,ttran0,ttrans,&
     &text,fext,fextt0,fexttr,dttr)
            integer :: np4
            integer :: np1
            integer :: nbm
            integer :: npf
            integer :: nttr
            integer :: ntrans
            real(kind=8) :: ttran0
            real(kind=8) :: ttrans
            real(kind=8) :: text(*)
            real(kind=8) :: fext(np4,*)
            real(kind=8) :: fextt0(*)
            real(kind=8) :: fexttr(*)
            real(kind=8) :: dttr
          end subroutine defttr
        end interface
