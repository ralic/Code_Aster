subroutine irmano(noma, nbma, numai, nbnos, numnos)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!
    include 'jeveux.h'
!
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: noma
    integer :: nbma, numai(*), nbnos, numnos(*)
! ----------------------------------------------------------------------
!     BUT :   TROUVER LA LISTE DES NUMEROS DE NOEUDS SOMMETS D'UNE LISTE
!             DE MAILLES
!     ENTREES:
!        NOMA   : NOM DU MAILLAGE
!        NBMA   : NOMBRE DE MAILLES DE LA LISTE
!        NUMAI  : NUMEROS DES MAILLES DE LA LISTE
!     SORTIES:
!        NBNOS  : NOMBRE DE NOEUDS SOMMETS
!        NUMNOS : NUMEROS DES NOEUDS SOMMETS (UN NOEUD APPARAIT UNE
!                               SEULE FOIS )
! ----------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: nnoe
    character(len=8) :: nomma, cbid
!
!
!-----------------------------------------------------------------------
    integer :: ier, ima, imai, ino, inoe, ipoin, jconx
    integer :: jnunos, jpoin, nbnoe, num
!-----------------------------------------------------------------------
    call jemarq()
    nomma=noma
    nbnos= 0
!  --- RECHERCHE DU NOMBRE DE NOEUDS DU MAILLAGE ---
!-DEL CALL JELIRA(NOMMA//'.NOMNOE','NOMMAX',NBNOE,' ')
    call dismoi('F', 'NB_NO_MAILLA', nomma, 'MAILLAGE', nbnoe,&
                cbid, ier)
    call wkvect('&&IRMANO.NUMNOS', 'V V I', nbnoe, jnunos)
!     --- INITIALISATION DU TABLEAU DE TRAVAIL &&IRMANO.NUMNOS ----
    do 10 ino = 1, nbnoe
        zi(jnunos-1+ino) = 0
10  end do
!     --- RECHERCHE DES NOEUDS SOMMETS ----
    call jeveuo(nomma//'.CONNEX', 'L', jconx)
    call jeveuo(jexatr(nomma//'.CONNEX', 'LONCUM'), 'L', jpoin)
    do 12 ima = 1, nbma
        imai=numai(ima)
        ipoin= zi(jpoin-1+imai)
        nnoe = zi(jpoin-1+imai+1)-ipoin
        do 22 inoe = 1, nnoe
            num=zi(jconx-1+ipoin-1+inoe)
            zi(jnunos-1+num) =1
22      continue
12  continue
!  --- STOCKAGE DES NOEUDS PRESENTS SUR LA LISTE DES MAILLES---
    do 32 inoe = 1, nbnoe
        if (zi(jnunos-1+inoe) .eq. 1) then
            nbnos=nbnos+1
            numnos(nbnos)=inoe
        endif
32  continue
!
    call jedetr('&&IRMANO.NUMNOS')
    call jedema()
end subroutine
