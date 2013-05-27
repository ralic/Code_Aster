        interface
          subroutine defext(np4,nbm,npfts,ndef,tc,textts,fextts,fmod,&
     &indt,niter)
            integer :: np4
            integer :: nbm
            integer :: npfts
            integer :: ndef
            real(kind=8) :: tc
            real(kind=8) :: textts(*)
            real(kind=8) :: fextts(np4,*)
            real(kind=8) :: fmod(*)
            integer :: indt
            integer :: niter
          end subroutine defext
        end interface
