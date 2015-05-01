subroutine cetule(model0, tbgrca, codret)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! GRANDEURS CARACTERISTIQUES DE L'ETUDE - LECTURE
!           *                     ***     **
!     ------------------------------------------------------------------
!      RECUPERATION DES GRANDEURS CARACTERISTIQUES CONTENUES
!      DANS LE MODELE
!      QUAND ELLES SONT FOURNIES, CES GRANDEURS SONT > 0
!      PAR DEFAUT, ON MET DONC DES VALEURS NEGATIVES POUR TESTER ENSUITE
!      REMARQUE : LES GRANDEURS SONT STOCKEES PAR LE SP CETUCR
!
! IN  : MODELE  : NOM DE LA SD MODELE
! OUT : TBGRCA  : TABLEAU DES GRANDEURS CARACTERISTIQUES AVEC LA
!                 CONVENTION SUIVANTE :
!                 1 : LONGUEUR
!                 2 : PRESSION
!                 3 : TEMPERATURE
! OUT : CODRET  : 0 : OK
!                 1 : LA TABLE N'EXISTE PAS
!                 2 : UN DES PARAMETRES EST DEFINI 0 OU PLUSIEURS FOIS
!     ------------------------------------------------------------------
!
    implicit   none
!
! DECLARATION PARAMETRES
!
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/ltnotb.h"
#include "asterfort/tbliva.h"
    integer :: nbmcle
    parameter  ( nbmcle = 3 )
!
! DECLARATION PARAMETRES D'APPELS
! -------------------------------
    character(len=*) :: model0
    real(kind=8) :: tbgrca(nbmcle)
    integer :: codret
!     ------------------------------------------------------------------
!
! DECLARATION VARIABLES LOCALES
!
    integer :: ibid, iaux, vali, iret
!
    real(kind=8) :: valeur, rbid
!
    complex(kind=8) :: cbid, valc
!
    character(len=1) :: ctype
    character(len=8) :: nomgrd(nbmcle)
    character(len=8) :: modele
    character(len=8) :: kbid, valk
    character(len=19) :: table
!
!     ------------------------------------------------------------------
    data nomgrd / 'LONGUEUR', 'PRESSION', 'TEMP' /
!     ------------------------------------------------------------------
!
    call jemarq()
    ibid=0
    rbid=0.d0
    cbid=(0.d0,0.d0)
!
    modele = model0(1:8)
!
!====
! 1. VALEURS PAR DEFAUT (R8VIDE ? VOIR TE0500)
!====
!
    do 10 , iaux = 1 , nbmcle
    tbgrca(iaux) = -1.d0
    10 end do
!
!====
! 2. REPERAGE DE LA TABLE
!====
!
    call jeexin(modele//'           .LTNT', iret)
    if (iret .ne. 0) then
        call ltnotb(modele, 'CARA_ETUDE', table)
        codret = 0
    else
        codret = 1
    endif
!
!====
! 3. DECODAGE DE LA TABLE
!====
!
    if (codret .eq. 0) then
!
        do 30 , iaux = 1 , nbmcle
!
        call tbliva(table, 1, 'GRANDEUR', [ibid], [rbid],&
                    [cbid], nomgrd( iaux), kbid, [rbid], 'VALE',&
                    ctype, vali, valeur, valc, valk,&
                    iret)
!
        if (iret .eq. 0) then
!
            tbgrca(iaux) = valeur
!
        else if (iret.ge.2) then
!
            codret = 2
!
        endif
!
30      continue
!
    endif
!
    call jedema()
!
end subroutine
