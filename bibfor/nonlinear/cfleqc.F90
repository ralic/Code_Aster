subroutine cfleqc(noma, defico, nzoco, nnoco, nsuco,&
                  poinsn, indino, elimno)
!
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
!
    include 'asterfort/assert.h'
    include 'asterfort/cfnbsf.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/mminfl.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: noma
    integer :: nzoco, nnoco, nsuco
    character(len=24) :: defico
    character(len=24) :: poinsn, indino
    integer :: elimno
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - LECTURE DONNEES - QUAD8)
!
! LSITE DES NOEUDS MILIEUX A ELIMINER
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : NOM SD CONTACT DEFINITION
! IN  NZOCO  : NOMBRE TOTAL DE ZONES DE CONTACT
! IN  NNOCO  : NOMBRE TOTAL DE NOEUDS DE CONTACT
! IN  NSUCO  : NOMBRE TOTAL DE SURFACES DE CONTACT
! OUT ELIMNO : NOMBRE DE NOEUDS A ELIMINER
! OUT POINSN : POINTEUR MISE A JOUR POUR INDINO
! OUT INDINO : NOEUDS A ELIMINER POUR CHAQUE SURFACE
!
!
!
!
    character(len=24) :: pzone
    integer :: jzone
    character(len=24) :: contma, contno, conoqu, pnoqua
    integer :: jmaco, jnoco, jnoqu, jnoqua
    integer :: jdecma, nummai, posmai
    integer :: isurf, izone, ima, ino, nutyp, inoqua, isuco
    integer :: nbsurf, nbma, nbnoq, nbno
    integer :: iatyma, itypma
    character(len=8) :: nomtm
    logical :: lveri
    integer :: jindno, jelino
    integer :: jdecno, jdecqu
    integer :: posno, numno1, numno2
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- CREATION DES SD
!
    call wkvect(indino, 'V V I', nnoco, jindno)
    call wkvect(poinsn, 'V V I', nsuco+1, jelino)
!
! --- ACCES AUX STRUCTURES DE DONNEES DE CONTACT
!
    conoqu = defico(1:16)//'.NOEUQU'
    call jeveuo(conoqu, 'L', jnoqu)
    pnoqua = defico(1:16)//'.PNOEUQU'
    call jeveuo(pnoqua, 'L', jnoqua)
    contma = defico(1:16)//'.MAILCO'
    contno = defico(1:16)//'.NOEUCO'
    pzone = defico(1:16)//'.PZONECO'
    call jeveuo(contma, 'L', jmaco)
    call jeveuo(contno, 'L', jnoco)
    call jeveuo(pzone, 'L', jzone)
!
! --- INITIALISATIONS
!
    elimno = 0
    jdecqu = 0
    call jeveuo(noma(1:8)//'.TYPMAIL', 'L', iatyma)
!
! --- LISTE DES NOEUDS MILIEUX POUR CHAQUE ZONE
!
    do 10 izone = 1, nzoco
!
        lveri = mminfl(defico,'VERIF',izone )
        if (lveri) then
            goto 21
        endif
!
! ----- NOMBRE DE SURFACES DE CONTACT
!
        nbsurf = zi(jzone+izone) - zi(jzone+izone-1)
        call assert(nbsurf.eq.2)
!
        do 20 isuco = 1, nbsurf
            isurf = nbsurf*(izone-1)+isuco
!
            zi(jelino+isurf) = zi(jelino+isurf-1)
!
            call cfnbsf(defico, isurf, 'MAIL', nbma, jdecma)
!
            do 30 ima = 1, nbma
!
! --------- NUMERO MAILLE COURANTE
!
                posmai = jdecma+ima
                nummai = zi(jmaco+posmai-1)
!
! --------- TYPE MAILLE COURANTE
!
                itypma = iatyma - 1 + nummai
                nutyp = zi(itypma)
                call jenuno(jexnum('&CATA.TM.NOMTM', nutyp ), nomtm)
!
! --------- ELIMINATION DES NOEUDS MILIEUX DES ARETES DES QUAD8
!
                if (nomtm(1:5) .eq. 'QUAD8') then
                    nbnoq = zi(jnoqua+izone) - zi(jnoqua+izone-1)
                    jdecqu = zi(jnoqua+izone-1)
                    call cfnbsf(defico, isurf, 'NOEU', nbno, jdecno)
                    nbnoq = nbnoq/3
                    do 70 inoqua = 1, nbnoq
                        numno1 = zi(jnoqu+jdecqu+3*(inoqua-1)+1-1)
                        do 80 ino = 1, nbno
                            posno = jdecno+ino
                            numno2 = zi(jnoco+posno-1)
                            if (numno1 .eq. numno2) then
                                if (zi(jindno+posno-1) .eq. 0) then
                                    zi(jindno+posno-1) = 1
                                    zi(jelino+isurf) = zi(jelino+ isurf)+1
                                    elimno = elimno + 1
                                endif
                            endif
80                      continue
70                  continue
                endif
30          continue
20      continue
21      continue
10  end do
!
    call assert((2*nzoco).eq.nsuco)
!
    call jedema()
end subroutine
