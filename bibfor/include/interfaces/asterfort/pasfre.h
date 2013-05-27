        interface
          subroutine pasfre(disc,freq,pasf,dim,nbm,iv,imodi,freqi,&
     &freqf,nb)
            integer :: nb
            integer :: nbm
            integer :: dim
            real(kind=8) :: disc(2,*)
            real(kind=8) :: freq(2,nbm,*)
            real(kind=8) :: pasf(dim*nb)
            integer :: iv
            integer :: imodi
            real(kind=8) :: freqi
            real(kind=8) :: freqf
          end subroutine pasfre
        end interface
