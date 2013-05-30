subroutine surfun(char, noma)
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
!
    implicit    none
    include 'jeveux.h'
!
    include 'asterfort/cudisi.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    character(len=8) :: char
    character(len=8) :: noma
!
! ----------------------------------------------------------------------
!
! ROUTINE LIAISON_UNILATERALE (AFFICHAGE DONNEES)
!
! AFFICHAGE DES RESULTATS DE LA LECTURE DU MOT-CLE LIAISON_UNILATERALE
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  NOMA   : NOM DU MAILLAGE
!
!
!
!
    integer :: ifm, niv
    character(len=24) :: cmpgcu, coegcu, coedcu, poincu, noeucu
    integer :: jcmpg, jcoefg, jcoefd, jpoin, jnoeu
    integer :: nnocu, ncmpg
    integer :: numno, nbcmp, jdecal
    character(len=24) :: noeuma, deficu
    integer :: ino, icmp
    integer :: lgbloc
    character(len=8) :: nomno
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
! --- INITIALISATIONS
!
    deficu = char(1:8)//'.UNILATE'
    noeuma = noma // '.NOMNOE'
    lgbloc = cudisi(deficu,'NB_RESOL')
    nnocu = cudisi(deficu,'NNOCU')
    ncmpg = cudisi(deficu,'NCMPG')
!
! --- ACCES SD
!
    noeuma = noma // '.NOMNOE'
    noeucu = deficu(1:16)//'.LISNOE'
    poincu = deficu(1:16)//'.POINOE'
    cmpgcu = deficu(1:16)//'.CMPGCU'
    coegcu = deficu(1:16)//'.COEFG'
    coedcu = deficu(1:16)//'.COEFD'
    call jeveuo(noeucu, 'L', jnoeu)
    call jeveuo(poincu, 'L', jpoin)
    call jeveuo(cmpgcu, 'L', jcmpg)
    call jeveuo(coegcu, 'L', jcoefg)
    call jeveuo(coedcu, 'L', jcoefd)
!
! ======================================================================
!                    IMPRESSIONS POUR L'UTILISATEUR
! ======================================================================
!
    if (niv .ge. 2) then
!
!
! --- IMPRESSIONS POUR L'UTILISATEUR
!
        write (ifm,*)
        write (ifm,*) '<LIA_UNIL> INFOS GENERALES'
        write (ifm,*)
!
        write (ifm,1070) 'NB_RESOL        ',lgbloc
!
! --- INFOS GENERALES
!
        write (ifm,*)
        write (ifm,1070) 'NNOCU           ',nnocu
        write (ifm,1070) 'NCMPG           ',ncmpg
        write (ifm,*)
!
!
        do 10 ino = 1, nnocu
!
            numno = zi(jnoeu+ino-1)
            call jenuno(jexnum(noeuma, numno), nomno)
!
!
            write (ifm,1030) nomno
            write (ifm,1031) ' --> INEGALITE ai.Ai<C : '
!
            nbcmp = zi(jpoin+ino) - zi(jpoin+ino-1)
            jdecal = zi(jpoin+ino-1)
!
            write (ifm,1040) '     (ai,Ai)'
!
            do 20 icmp = jdecal, jdecal+nbcmp-1
                write (ifm,1042) '     ( ',zk8(jcoefg-1+icmp),' , ',&
     &                         zk8(jcmpg-1+icmp),' )'
20          continue
            write (ifm,1060) '     (C)'
            write (ifm,1062) '     ( ',zk8(jcoefd-1+ino),' )'
10      continue
!
    endif
!
    1070 format (' <LIA_UNIL> ...... PARAM. : ',a16,' - VAL. : ',i5)
!
!
    1031 format ('<LIA_UNIL> ',a25)
    1030 format ('<LIA_UNIL> NOEUD: ',a18,a8)
    1040 format ('<LIA_UNIL>',a12)
    1042 format ('<LIA_UNIL>',a7,a8,a3,a8,a2)
    1060 format ('<LIA_UNIL>',a8)
    1062 format ('<LIA_UNIL>',a7,a8,a2)
!
    call jedetr('&&SURFUN.TRAV')
! ======================================================================
    call jedema()
!
end subroutine
