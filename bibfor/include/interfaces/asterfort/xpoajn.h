        interface
          subroutine xpoajn(maxfem,ino,lsn,jdirno,prefno,nfiss,he,nnn,&
     &inn,inntot,nbnoc,nbnofi,inofi,co,iacoo2)
            integer :: nfiss
            character(len=8) :: maxfem
            integer :: ino
            real(kind=8) :: lsn(nfiss)
            integer :: jdirno
            character(len=2) :: prefno(4)
            integer :: he(nfiss)
            integer :: nnn
            integer :: inn
            integer :: inntot
            integer :: nbnoc
            integer :: nbnofi
            integer :: inofi
            real(kind=8) :: co(3)
            integer :: iacoo2
          end subroutine xpoajn
        end interface
