        interface
          subroutine pipeei(ndim,axi,nno1,nno2,npg,wref,vff1,vff2,&
     &dffr2,geom,ang,mat,compor,lgpg,ddlm,ddld,ddl0,ddl1,dtau,vim,iu,im,&
     &copilo)
            integer :: lgpg
            integer :: npg
            integer :: nno2
            integer :: nno1
            integer :: ndim
            logical :: axi
            real(kind=8) :: wref(npg)
            real(kind=8) :: vff1(nno1,npg)
            real(kind=8) :: vff2(nno2,npg)
            real(kind=8) :: dffr2(ndim-1,nno2,npg)
            real(kind=8) :: geom(ndim,nno2)
            real(kind=8) :: ang(*)
            integer :: mat
            character(len=16) :: compor
            real(kind=8) :: ddlm(2*nno1*ndim+nno2*ndim)
            real(kind=8) :: ddld(2*nno1*ndim+nno2*ndim)
            real(kind=8) :: ddl0(2*nno1*ndim+nno2*ndim)
            real(kind=8) :: ddl1(2*nno1*ndim+nno2*ndim)
            real(kind=8) :: dtau
            real(kind=8) :: vim(lgpg,npg)
            integer :: iu(3,18)
            integer :: im(3,9)
            real(kind=8) :: copilo(5,npg)
          end subroutine pipeei
        end interface
