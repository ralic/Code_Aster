        interface
          subroutine nmcrit(nomsd,nbinst,typsel,nume,inst,freq,tole,&
     &lselec)
            character(len=19) :: nomsd
            integer :: nbinst
            character(len=4) :: typsel
            integer :: nume
            real(kind=8) :: inst
            integer :: freq
            real(kind=8) :: tole
            logical :: lselec
          end subroutine nmcrit
        end interface
