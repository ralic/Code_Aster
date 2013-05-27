subroutine ddlphy(depplu, neq, vect, desc)
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
! person_in_charge: ludovic.idoux at edf.fr
! ----------------------------------------------------------------------
!     CALCUL DES ENERGIES
! ----------------------------------------------------------------------
!  IN      : DEPPLU   : VECTEUR DES INCONNUS
!  IN      : NEQ      : DIMENSION DU VECTEUR
!  IN/OUT  : VECT     : EN ENTREE : VECTEUR COMPLET
!                       EN SORTIE : LES DDL NON PHYSIQUES SONT A ZERO
!  OUT     : DESC     : DESCRIPTEUR DES COMPOSANTES
!
! CORPS DU PROGRAMME
    implicit none
! DECLARATION PARAMETRES D'APPELS
!
    include 'jeveux.h'
!
    include 'asterfort/dismoi.h'
    include 'asterfort/iposdg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/wkvect.h'
    real(kind=8) :: vect(*)
    character(len=19) :: depplu
    character(len=8) :: desc(*)
    integer :: neq
!
!
! DECLARATION VARIABLES LOCALES
!
    character(len=8) :: nomgd, kbid, noma, exclus(200)
    character(len=19) :: prno
    integer :: iaux, ier, jnocmp, ncmpmx, jprno, jnueq, nec
    integer :: jdg, inueq, jaux, kaux
    integer :: nbnot, ibid
    integer :: nbexcl, ival, pos, ivect2
    logical :: garder
!
    call jemarq()
!
    do 5 iaux = 1, 200
        exclus(iaux)=' '
 5  end do
    call wkvect('VECTMP', 'V V R', neq, ivect2)
    do 10 iaux = 1, neq
        zr(ivect2-1+iaux)=vect(iaux)
        vect(iaux)=0.d0
        desc(iaux)=' '
10  end do
    call dismoi('F', 'NOM_GD', depplu, 'CHAM_NO', ibid,&
                nomgd, ier)
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomgd), 'L', jnocmp)
    call dismoi('F', 'NB_CMP_MAX', nomgd, 'GRANDEUR', ncmpmx,&
                kbid, ier)
    call dismoi('F', 'PROF_CHNO', depplu, 'CHAM_NO', ibid,&
                prno, ier)
    call jeveuo(jexnum(prno//'.PRNO', 1), 'L', jprno)
    call jeveuo(prno//'.NUEQ', 'L', jnueq)
    call dismoi('F', 'NB_EC', nomgd, 'GRANDEUR', nec,&
                kbid, ier)
    call dismoi('F', 'NOM_MAILLA', depplu, 'CHAM_NO', ibid,&
                noma, ier)
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbnot,&
                kbid, ier)
    nbexcl=0
!
    do 20 iaux = 1, nbnot
!       DEBUT DU DESCRIPTEUR GRANDEUR DU NOEUD
        jdg = jprno - 1 + (iaux-1)*(2+nec) + 1 + 2
!       INDIRECTION VERS LE .NUEQ
        inueq = zi(jprno - 1 + (iaux-1)*(2+nec) + 1)
!       POSITIONS DES DDL PHYSIQUES DANS LE DG
        do 21 jaux = 1, ncmpmx
            pos = iposdg(zi(jdg),jaux)
            if (pos .ne. 0) then
!           A EXCLURE OU CONSERVER
                garder=.true.
                do 22 kaux = 1, nbexcl
                    if (exclus(kaux) .eq. zk8(jnocmp-1+jaux)) then
                        garder=.false.
                    endif
22              continue
!           ADRESSE DU DDL DANS LE .VALE
                ival = zi(jnueq - 1 + inueq - 1 + pos)
                desc(ival) = zk8(jnocmp-1+jaux)
                if (garder) then
                    vect(ival) = zr(ivect2-1+ival)
                endif
            endif
21      continue
20  end do
!
    call jedetr('VECTMP')
!
    call jedema()
!
end subroutine
