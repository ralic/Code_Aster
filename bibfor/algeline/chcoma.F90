subroutine chcoma(tablez, nomaou)
    implicit   none
#include "jeveux.h"
#include "asterc/getvtx.h"
#include "asterc/r8dgrd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/tbexp2.h"
#include "asterfort/tbliva.h"
#include "asterfort/u2mess.h"
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
!      CHCOMA -- IL S'AGIT DE CHANGER LES VALEURS DES COORDONNEES
!                DES NOEUDS DU MAILLAGE DE NOM NOMAOU QUI SONT EXPRIMEES
!                DANS LE REPERE GLOBAL EN LEURS VALEURS EXPRIMEES
!                DANS LE REPERE PRINCIPAL D'INERTIE DE CE MAILLAGE.
!                TRAVAILLER AVEC UN MAILLAGE DONT LES COORDONNEES
!                DES NOEUDS SONT EXPRIMEES DANS LE REPERE PRINCIPAL
!                D'INERTIE EST NECESSAIRE POUR CALCULER LES
!                COEFFICIENTS DE CISAILLEMENT D'UNE POUTRE DONT
!                UNE SECTION EST REPRESENTEE PAR LE MAILLAGE
!                NOMAOU QUI EST CONSTITUE D'ELEMENTS MASSIFS 2D.
!
!
!   ARGUMENT        E/S  TYPE         ROLE
!    TABLEZ         IN    K*      NOM D'UNE TABLE DE TYPE TABL_CARA_GEOM
!                                 ISSUE DE LA COMMANDE POST_ELEM.
!                                 CETTE TABLE CONTIENT LES COORDONNEES
!                                 DE L'ORIGINE DU NOUVEAU REPERE
!                                 (I.E. LE CENTRE DE GRAVITE DE LA
!                                       SECTION)
!                                 ET L'ANGLE FORME PAR LES NOUVEAUX AXES
!                                 (I.E. LES AXES PRINCIPAUX D'INERTIE)
!                                 AVEC LES AXES GLOBAUX.
!    NOMAOU         IN    K*      NOM DU MAILLAGE REPRESENTANT LA
!                                 SECTION DE LA POUTRE MAILLEE AVEC
!                                 DES ELEMENTS MASSIFS 2D, LES
!                                 COORDONNEES DES NOEUDS ETANT DEFINIES
!                                 DANS LE REPERE GLOBAL EN ENTREE
!                                 DE LA ROUTINE ET DANS LE REPERE
!                                 PRINCIPAL D'INERTIE A LA SORTIE DE LA
!                                 ROUTINE.
!.========================= DEBUT DES DECLARATIONS ====================
! -----  VARIABLES LOCALES
    integer :: ngm, ibid, iret, idcode, dimcoo, nbno, jcoor, idcoor, ino
    real(kind=8) :: r8b, p(2, 2), alpha, yg, zg, yabs, zabs
    complex(kind=8) :: c16b
    character(len=8) :: k8b
    character(len=19) :: table
    character(len=24) :: cooval, coodes, nogrma, noma
    integer :: iarg
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
! --- VERIFICATION DES PARARAMETRES DE LA TABLE
!     -----------------------------------------
    call tbexp2(table, 'LIEU')
    call tbexp2(table, 'CDG_Y')
    call tbexp2(table, 'CDG_Z')
    call tbexp2(table, 'ALPHA')
!
! --- RECUPERATION DANS LA TABLE DES COORDONNEES DU CENTRE DE GRAVITE :
!     ---------------------------------------------------------------
    call getvtx('REPERE', 'GROUP_MA', 1, iarg, 0,&
                k8b, ngm)
    if (ngm .ne. 0) then
        ngm = 1
        call getvtx('REPERE', 'GROUP_MA', 1, iarg, ngm,&
                    nogrma, ngm)
        noma=nogrma
        iret=0
    else
        call tbexp2(table, 'MAILLAGE')
        call tbliva(table, 0, k8b, ibid, r8b,&
                    c16b, k8b, k8b, r8b, 'MAILLAGE',&
                    k8b, ibid, r8b, c16b, noma,&
                    iret)
    endif
    if (iret .ne. 0) call u2mess('F', 'MODELISA2_89')
    call tbliva(table, 1, 'LIEU', ibid, r8b,&
                c16b, noma, k8b, r8b, 'CDG_Y',&
                k8b, ibid, yg, c16b, k8b,&
                iret)
    print*,'IRET ',iret
    if (iret .ne. 0) call u2mess('F', 'MODELISA2_89')
    call tbliva(table, 1, 'LIEU', ibid, r8b,&
                c16b, noma, k8b, r8b, 'CDG_Z',&
                k8b, ibid, zg, c16b, k8b,&
                iret)
    if (iret .ne. 0) call u2mess('F', 'MODELISA2_89')
!
! --- RECUPERATION DANS LA TABLE DE L'ANGLE FAISANT PASSER DU REPERE
! --- PRINCIPAL D'INERTIE AU REPERE GLOBAL :
!     ------------------------------------
    call tbliva(table, 1, 'LIEU', ibid, r8b,&
                c16b, noma, k8b, r8b, 'ALPHA',&
                k8b, ibid, alpha, c16b, k8b,&
                iret)
    if (iret .ne. 0) call u2mess('F', 'ALGELINE_7')
!
! --- PASSAGE DE L'ANGLE DE DEGRES EN RADIANS :
!     ---------------------------------------
    alpha = alpha * r8dgrd()
!
! --- CONSTITUTION DE LA MATRICE DE PASSAGE DU REPERE GLOBAL
! --- AU REPERE D'INERTIE :
!     -------------------
    p(1,1) = cos(alpha)
    p(2,1) = sin(alpha)
    p(1,2) = -sin(alpha)
    p(2,2) = cos(alpha)
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
        zr(idcoor+1) = zr(idcoor+1) - yg
        zr(idcoor+2) = zr(idcoor+2) - zg
10  end do
!
! --- ROTATION D'ANGLE ALPHA DES AXES :
!     -------------------------------
    do 20 ino = 1, nbno
!
        idcoor = jcoor-1+dimcoo*(ino-1)
        yabs = zr(idcoor+1)
        zabs = zr(idcoor+2)
!
        zr(idcoor+1) = p(1,1)*yabs + p(2,1)*zabs
        zr(idcoor+2) = p(1,2)*yabs + p(2,2)*zabs
20  end do
!
    call jedema()
!.============================ FIN DE LA ROUTINE ======================
end subroutine
