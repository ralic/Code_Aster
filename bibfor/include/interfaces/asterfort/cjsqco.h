        interface
          subroutine cjsqco(gamma,sig,x,pref,epssig,i1,s,sii,siirel,&
     &cos3ts,hts,dets,q,qii,qiirel,cos3tq,htq,detq)
            real(kind=8) :: gamma
            real(kind=8) :: sig(6)
            real(kind=8) :: x(6)
            real(kind=8) :: pref
            real(kind=8) :: epssig
            real(kind=8) :: i1
            real(kind=8) :: s(6)
            real(kind=8) :: sii
            real(kind=8) :: siirel
            real(kind=8) :: cos3ts
            real(kind=8) :: hts
            real(kind=8) :: dets
            real(kind=8) :: q(6)
            real(kind=8) :: qii
            real(kind=8) :: qiirel
            real(kind=8) :: cos3tq
            real(kind=8) :: htq
            real(kind=8) :: detq
          end subroutine cjsqco
        end interface
