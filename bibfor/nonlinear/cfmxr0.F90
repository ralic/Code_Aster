subroutine cfmxr0(defico, resoco, noma)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/cfdisi.h'
    include 'asterfort/cfdisl.h'
    include 'asterfort/cfmmvd.h'
    include 'asterfort/cfnumn.h'
    include 'asterfort/cnscno.h'
    include 'asterfort/cnscre.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mminfi.h'
    include 'asterfort/wkvect.h'
    character(len=24) :: defico, resoco
    character(len=8) :: noma
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - POST-TRAITEMENT)
!
! CREER LE VALE_CONT POUR L'ARCHIVAGE DU CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  NOMA   : NOM DU MAILLAGE
!
!
!
!
    integer :: nbcmp
    parameter    (nbcmp = 30)
    character(len=8) :: nomcmp(nbcmp)
    integer :: nbper
    parameter    (nbper = 4)
    character(len=8) :: nomper(nbper)
!
    integer :: zresu, zperc
    integer :: ifm, niv
    integer :: izone, icmp, inoe, ibid
    integer :: nbnoe, posnoe, numnoe
    integer :: nbno, ino, numno
    integer :: nzoco
    character(len=8) :: k8bid
    character(len=24) :: nochco
    integer :: jnochc
    character(len=19) :: cnsinr, cnsper, cnoinr
    integer :: jcnsvr, jcnslr
    integer :: jcnsvp, jcnslp
    integer :: jdecne
    logical :: lctcc, lctcd, lmail
! ----------------------------------------------------------------------
    data nomcmp&
     &   / 'CONT'  ,'JEU'   ,'RN'    ,&
     &     'RNX'   ,'RNY'   ,'RNZ'   ,&
     &     'GLIX'  ,'GLIY'  ,'GLI'   ,&
     &     'RTAX'  ,'RTAY'  ,'RTAZ'  ,&
     &     'RTGX'  ,'RTGY'  ,'RTGZ'  ,&
     &     'RX'    ,'RY'    ,'RZ'    ,&
     &     'R'     ,'HN'    ,'I'     ,&
     &     'IX'    ,'IY'    ,'IZ'    ,&
     &     'PT_X'  ,'PT_Y'  ,'PT_Z'  ,&
     &     'PROJ_X','PROJ_Y','PROJ_Z'/
! ----------------------------------------------------------------------
    data nomper&
     &   / 'V1','V2','V3','V4'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- INITIALISATIONS
!
    nzoco = cfdisi(defico,'NZOCO' )
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbno,&
                k8bid, ibid)
    cnsinr = '&&CFMXR0.CNSINR'
    cnoinr = '&&CFMXR0.CNOINR'
    cnsper = '&&CFMXR0.CNSPER'
!
! --- NOM DES CHAM_NO
!
    nochco = resoco(1:14)//'.NOCHCO'
    call wkvect(nochco, 'V V K24', 3, jnochc)
    zk24(jnochc+1-1) = cnsinr
    zk24(jnochc+2-1) = cnoinr
    zk24(jnochc+3-1) = cnsper
!
! --- TAILLES
!
    zresu = cfmmvd('ZRESU')
    zperc = cfmmvd('ZPERC')
    if (zresu .ne. nbcmp) then
        call assert(.false.)
    endif
    if (zperc .ne. nbper) then
        call assert(.false.)
    endif
!
! --- TYPE DE CONTACT
!
    lctcc = cfdisl(defico,'FORMUL_CONTINUE')
    lctcd = cfdisl(defico,'FORMUL_DISCRETE')
    lmail = lctcc.or.lctcd
!
! --- CREATION DU CHAM_NO_S VALE_CONT
!
    call cnscre(noma, 'INFC_R', zresu, nomcmp, 'V',&
                cnsinr)
    call jeveuo(cnsinr(1:19)//'.CNSV', 'E', jcnsvr)
    call jeveuo(cnsinr(1:19)//'.CNSL', 'E', jcnslr)
!
! --- INITIALISATION DU CHAM_NO_S VALE_CONT
!
    if (lmail) then
        do 10 izone = 1, nzoco
            jdecne = mminfi(defico,'JDECNE',izone )
            nbnoe = mminfi(defico,'NBNOE' ,izone )
            do 11 inoe = 1, nbnoe
                posnoe = inoe + jdecne
                call cfnumn(defico, 1, posnoe, numnoe)
                do 12 icmp = 1, zresu
                    zr(jcnsvr-1+zresu*(numnoe-1)+icmp) = 0.d0
                    zl(jcnslr-1+zresu*(numnoe-1)+icmp) = .true.
12              continue
11          continue
10      continue
    else
        do 113 ino = 1, nbno
            numno = ino
            do 123 icmp = 1, zresu
                zr(jcnsvr-1+zresu*(numno-1)+icmp) = 0.d0
                zl(jcnslr-1+zresu*(numno-1)+icmp) = .true.
123          continue
113      continue
    endif
!
! --- CREATION DU CHAM_NO_S PERCUSSION
!
    if (lmail) then
        call cnscre(noma, 'VARI_R', zperc, nomper, 'V',&
                    cnsper)
    endif
!
! --- INITIALISATION DU CHAM_NO_S PERCUSSION
! --- ON NE REMET PAS A ZERO D'UN PAS A L'AUTRE
!
    if (lmail) then
        call jeveuo(cnsper(1:19)//'.CNSV', 'E', jcnsvp)
        call jeveuo(cnsper(1:19)//'.CNSL', 'E', jcnslp)
        do 1 izone = 1, nzoco
            jdecne = mminfi(defico,'JDECNE',izone )
            nbnoe = mminfi(defico,'NBNOE' ,izone )
            do 2 inoe = 1, nbnoe
                posnoe = inoe + jdecne
                call cfnumn(defico, 1, posnoe, numnoe)
                do 3 icmp = 1, zperc
                    zr(jcnsvp-1+zperc*(numnoe-1)+icmp) = 0.d0
                    zl(jcnslp-1+zperc*(numnoe-1)+icmp) = .false.
 3              continue
 2          continue
 1      continue
    endif
!
! --- TRANSFO. EN CHAM_NO
!
    call cnscno(cnsinr, ' ', 'NON', 'V', cnoinr,&
                'F', ibid)
!
    call jedema()
end subroutine
