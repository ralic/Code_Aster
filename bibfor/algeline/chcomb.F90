subroutine chcomb(tablez, nomaou)
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/tbexp2.h"
#include "asterfort/tbliva.h"
#include "asterfort/utmess.h"
    character(len=8) :: nomaou
    character(len=*) :: tablez
!.======================================================================
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
!      CHCOMB -- IL S'AGIT DE CHANGER LES VALEURS DES COORDONNEES
!                DES NOEUDS DU MAILLAGE DE NOM NOMAOU QUI SONT EXPRIMEES
!                DANS LE REPERE PRINCIPAL D'INERTIE AVEC POUR ORIGINE
!                LE CENTRE DE GRAVITE DE LA SECTION EN LEURS VALEURS
!                EXPRIMEES DANS UN REPERE AYANT LES MEMES DIRECTIONS
!                MAIS DONT L'ORIGINE EST SITUEE AU CENTRE DE
!                CISAILLEMENT-TORSION.
!                CE CHANGEMENT DE COORDONNEES EST NECESSAIRE
!                POUR CALCULER L'INERTIE DE GAUCHISSEMENT D'UNE POUTRE
!                DONT LA SECTION EST REPRESENTEE PAR LE MAILLAGE
!                NOMAOU QUI EST CONSTITUE D'ELEMENTS MASSIFS 2D.
!
!
!   ARGUMENT        E/S  TYPE         ROLE
!    TABLEZ         IN    K*      NOM D'UNE TABLE DE TYPE TABL_CARA_GEOM
!                                 ISSUE DE LA COMMANDE POST_ELEM.
!                                 CETTE TABLE CONTIENT LES COORDONNEES
!                                 DU CENTRE DE CISAILLEMENT-TORSION.
!    NOMAOU         IN    K*      NOM DU MAILLAGE REPRESENTANT LA
!                                 SECTION DE LA POUTRE MAILLEE AVEC
!                                 DES ELEMENTS MASSIFS 2D, LES
!                                 COORDONNEES DES NOEUDS ETANT DEFINIES
!                                 DANS LE REPERE PRINCIPAL D'INERTIE
!                                 DONT L'ORIGINE EST LE CENTRE DE
!                                 GRAVITE DE LA SECTION EN ENTREE
!                                 DE LA ROUTINE ET DANS CE MEME REPERE
!                                 DONT L'ORIGINE EST SITUEE AU CENTRE
!                                 DE CISAILLEMENT-TORSION EN SORTIE.
!.========================= DEBUT DES DECLARATIONS ====================
! -----  VARIABLES LOCALES
    integer :: ibid, iret, idcode, dimcoo, nbno, jcoor, idcoor, ino
    real(kind=8) :: r8b, xt, yt
    complex(kind=8) :: c16b
    character(len=8) :: k8b, noma
    character(len=19) :: table
    character(len=24) :: cooval, coodes
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
    call jemarq()
!
! --- INITIALISATIONS :
!     ---------------
    table = tablez
    cooval = nomaou//'.COORDO    .VALE'
    coodes = nomaou//'.COORDO    .DESC'
!
! --- VERIFICATION DES PARAMETRES DE LA TABLE
!     ---------------------------------------
    call tbexp2(table, 'MAILLAGE')
    call tbexp2(table, 'LIEU')
    call tbexp2(table, 'EY')
    call tbexp2(table, 'EZ')
!
! --- RECUPERATION DANS LA TABLE DES COORDONNEES DU CENTRE DE
! --- DE CISAILLEMENT-TORSION :
!     -----------------------
    call tbliva(table, 0, k8b, [ibid], [r8b],&
                [c16b], k8b, k8b, [r8b], 'MAILLAGE',&
                k8b, ibid, r8b, c16b, noma,&
                iret)
    call tbliva(table, 1, 'LIEU', [ibid], [r8b],&
                [c16b], noma, k8b, [r8b], 'EY',&
                k8b, ibid, xt, c16b, k8b,&
                iret)
    if (iret .ne. 0) then
        call utmess('F', 'MODELISA2_89')
    endif
    call tbliva(table, 1, 'LIEU', [ibid], [r8b],&
                [c16b], noma, k8b, [r8b], 'EZ',&
                k8b, ibid, yt, c16b, k8b,&
                iret)
    if (iret .ne. 0) then
        call utmess('F', 'MODELISA2_89')
    endif
!
! --- RECUPERATION DE LA DIMENSION DU MAILLAGE :
!     ----------------------------------------
    call jeveuo(coodes, 'L', idcode)
    dimcoo = -zi(idcode+2-1)
!
! --- NOMBRE DE NOEUDS DU MAILLAGE :
!     ----------------------------
    call dismoi('F', 'NB_NO_MAILLA', nomaou, 'MAILLAGE', nbno,&
                k8b, iret)
!
! --- RECUPERATION DES COORDONNEES DES NOEUDS DU MAILLAGE :
!     ---------------------------------------------------
    call jeveuo(cooval, 'E', jcoor)
!
! --- CHANGEMENT D'ORIGINE DES COORDONNEES :
!     ------------------------------------
    do 10 ino = 1, nbno
!
        idcoor = jcoor-1+dimcoo*(ino-1)
        zr(idcoor+1) = zr(idcoor+1) + xt
        zr(idcoor+2) = zr(idcoor+2) + yt
10  end do
!
    call jedema()
!.============================ FIN DE LA ROUTINE ======================
end subroutine
