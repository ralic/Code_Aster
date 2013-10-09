subroutine numoch(tlimat, nbmat, base, lmoch)
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
!
    character(len=24) :: tlimat(*)
    integer :: nbmat
    character(len=1) :: base
    character(len=*) :: lmoch
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! ----------------------------------------------------------------------
! --- DESCRIPTION DES PARAMETRES
! IN  K24  TLIMAT : LISTE DES MATELE DEFINISSANT LA NUMEROTATION
! IN  I    NBMAT  : NOMBRE DE MATELE PASSES DANS TLIMAT
! IN  K1   BASE   : BASE SUR LAQUELLE ON CREE LMOCH
! OUT K*24 LMOCH  : L'OBJET DE NOM LMOCH EST CREE ET REMPLI, IL CONTIENT
!                  CREATION : BASE//' V K24'
!                  CONTENU  : LA LISTE DES NOMS DES LIGRELS DE MODELE OU
!                  CHARGE SUPPORTANT LA LISTE DE MATR_ELEM TLIMAT, SON
!                  ARGUMENT 'LONUTI' EST DIMENSIONNE EXACTEMENT A SON
!                  NOMBRE D'ELEMENTS
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!----------------------------------------------------------------------
!     VARIABLES LOCALES
!----------------------------------------------------------------------
    character(len=19) :: matel, nomli, resu
!----------------------------------------------------------------------
!                DEBUT DES INSTRUCTIONS
!----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: iad, ideja, idiml, idlres, ili
    integer :: ilmoch, imat, iresu, iret, n1, nbresu, nlmoch
!
!-----------------------------------------------------------------------
    call jemarq()
!
!---- CALCUL DU NBRE MAX DE MODELES ET DE CHARGES
!
    idiml = 2
    do 100 imat = 1, nbmat
        matel = tlimat(imat)
        call jeexin(matel//'.RELR', iret)
        if (iret .eq. 0) goto 100
!
        call jeveuo(matel//'.RELR', 'L', idlres)
        call jelira(matel//'.RELR', 'LONUTI', nbresu)
        idiml = idiml + nbresu
100 end do
!
!---- CREATION DU VECTEUR LMOCH
!
    call wkvect(lmoch, base//' V K24', idiml, ilmoch)
    nlmoch = 0
    do 110 imat = 1, nbmat
        matel = tlimat(imat)
        call dismoi('NB_SS_ACTI', matel, 'MATR_ELEM', repi=n1)
!
        if (n1 .gt. 0) then
            call dismoi('NOM_MODELE', matel, 'MATR_ELEM', repk=nomli)
            nomli=nomli(1:8)//'.MODELE'
            ideja =0
            do 1001 ili = 1, nlmoch
                if (nomli .eq. zk24(ilmoch-1+ili)) ideja = 1
1001         continue
            if (ideja .eq. 0) then
                nlmoch = nlmoch + 1
                zk24(ilmoch-1+nlmoch) = nomli
            endif
        endif
!
        call jeexin(matel//'.RELR', iret)
        if (iret .eq. 0) goto 110
!
        call jeveuo(matel//'.RELR', 'L', idlres)
        call jelira(matel//'.RELR', 'LONUTI', nbresu)
        do 120 iresu = 1, nbresu
            resu = zk24(idlres+iresu-1)
            call jeexin(resu//'.NOLI', iret)
            if (iret .eq. 0) goto 120
            call jeveuo(resu//'.NOLI', 'L', iad)
            nomli = zk24(iad)
            ideja =0
            do 1000 ili = 1, nlmoch
                if (nomli .eq. zk24(ilmoch-1+ili)) ideja = 1
1000         continue
            if (ideja .eq. 0) then
                nlmoch = nlmoch + 1
                zk24(ilmoch-1+nlmoch) = nomli
            endif
120     continue
110 end do
    call jeecra(lmoch, 'LONUTI', nlmoch)
    call jedema()
end subroutine
