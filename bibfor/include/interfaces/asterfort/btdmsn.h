        interface
          subroutine btdmsn(ind,nb1,intsn,npgsr,xr,btdm,btdf,btds,&
     &btild)
            integer :: ind
            integer :: nb1
            integer :: intsn
            integer :: npgsr
            real(kind=8) :: xr(*)
            real(kind=8) :: btdm(4,3,42)
            real(kind=8) :: btdf(3,42)
            real(kind=8) :: btds(4,2,42)
            real(kind=8) :: btild(5,42)
          end subroutine btdmsn
        end interface
