subroutine lrmtyp(nbtyp, nomtyp, nnotyp, typgeo, renumd,&
                  modnum, nuanom, numnoa)
!_____________________________________________________________________
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
! person_in_charge: nicolas.sellenet at edf.fr
!     RECUP DES NOMS/NBNO DES TYPES DE MAILLES DANS LE CATALOGUE
!     ET RECUP DES TYPE GEO CORRESPONDANT POUR MED
!
!     SORTIE:
!       MODNUM : INDICATEUR SI LA SPECIFICATION DE NUMEROTATION DES
!                NOEUDS DES MAILLES EST DIFFERENTES ENTRE ASTER ET MED:
!                     MODNUM = 0 : NUMEROTATION IDENTIQUE
!                     MODNUM = 1 : NUMEROTATION DIFFERENTE
!       NUANOM : TABLEAU DE CORRESPONDANCE DES NOEUDS (MED/ASTER).
!                NUANOM(ITYP,J): NUMERO DANS ASTER DU J IEME NOEUD
!                DE LA MAILLE DE TYPE ITYP DANS MED.
!       NUMNOA : TABLEAU DE CORRESPONDANCE DES NOEUDS (MED/ASTER).
!                NUMNOA(ITYP,J) : NUMERO DANS MED DU J IEME NOEUD
!                DE LA MAILLE DE TYPE ITYP D'ASTER
!
! ---------------------------------------------------------------------
    implicit none
!
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mess.h"
    integer :: ntymax
    parameter (ntymax = 69)
    integer :: nnomax
    parameter (nnomax=27)
!
! 0.1. ==> ARGUMENTS
!
    integer :: nbtyp
    integer :: nnotyp(ntymax), typgeo(ntymax), renumd(ntymax)
    integer :: modnum(ntymax)
    integer :: nuanom(ntymax, nnomax), numnoa(ntymax, nnomax)
    character(len=8) :: nomtyp(ntymax)
!
! 0.2. ==> COMMUNS
!
!
! 0.3. ==> VARIABLES LOCALES
!
!
    integer :: nummed(ntymax)
    integer :: iaux, jaux
    integer :: ityp
    character(len=8) :: nomast(ntymax)
    character(len=1) :: k1
!
! 0.4. ==> INITIALISATIONS
!
!     CORRESPONDANCE DES NUMEROS DE TYPE DE GEOMETRIE ENTRE ASTER ET MED
!     (LIE A LORDRE DEFINI DANS LE CATALOGUE TYPE_MAILLE.CATA)
    data nomast  /'POI1    ','SEG2    ','SEG22   ','SEG3    ',&
     &              'SEG33   ','SEG4    ',&
     &                         'TRIA3   ','TRIA33  ','TRIA6   ',&
     &              'TRIA66  ','TRIA7   ','QUAD4   ','QUAD44  ',&
     &              'QUAD8   ','QUAD88  ','QUAD9   ','QUAD99  ',&
     &              'TETRA4  ','TETRA10 ','PENTA6  ','PENTA15 ',&
     &              'PENTA18 ','PYRAM5  ','PYRAM13 ','HEXA8   ',&
     &              'HEXA20  ','HEXA27  ','TR3QU4  ','QU4TR3  ',&
     &              'TR6TR3  ','TR3TR6  ','TR6QU4  ','QU4TR6  ',&
     &              'TR6QU8  ','QU8TR6  ','TR6QU9  ','QU9TR6  ',&
     &              'QU8TR3  ','TR3QU8  ','QU8QU4  ','QU4QU8  ',&
     &              'QU8QU9  ','QU9QU8  ','QU9QU4  ','QU4QU9  ',&
     &              'QU9TR3  ','TR3QU9  ','SEG32   ','SEG23   ',&
     &              'QU4QU4  ','TR3TR3  ','HE8HE8  ','PE6PE6  ',&
     &              'TE4TE4  ','QU8QU8  ','TR6TR6  ','SE2TR3  ',&
     &              'SE2TR6  ','SE2QU4  ','SE2QU8  ','SE2QU9  ',&
     &              'SE3TR3  ','SE3TR6  ','SE3QU4  ','SE3QU8  ',&
     &              'SE3QU9  ','H20H20  ','P15P15  ','T10T10  '/
    data nummed  /1,         102,       0,         103,&
     &              0,         104,&
     &                         203,       0,         206,&
     &              0,         207,       204,       0,&
     &              208,       0,         209,       0,&
     &              304,       310,       306,       315,&
     &              0,         305,       313,       308,&
     &              320,       327,       0,         0,&
     &              0,         0,         0,         0,&
     &              0,         0,         0,         0,&
     &              0,         0,         0,         0,&
     &              0,         0,         0,         0,&
     &              0,         0,         0,         0,&
     &              0,         0,         0,         0,&
     &              0,         0,         0,         0,&
     &              0,         0,         0,         0,&
     &              0,         0,         0,         0,&
     &              0,         0,         0,         0/
!     ------------------------------------------------------------------
    call jemarq()
!
!     VERIFICATION QUE LE CATALOGUE EST ENCORE COHERENT AVEC LE FORTRAN
!
    call jelira('&CATA.TM.NOMTM', 'NOMMAX', iaux, k1)
    if (ntymax .ne. iaux) then
        call u2mess('F', 'MED_38')
    endif
!
!     NOM / NBNO PAR TYPE DE MAILLE
!
    do 1 ityp = 1, ntymax
        call jenuno(jexnum('&CATA.TM.NOMTM', ityp), nomtyp(ityp))
        if (nomast(ityp) .ne. nomtyp(ityp)) then
            call u2mess('F', 'MED_39')
        endif
        call jeveuo(jexnum('&CATA.TM.NBNO' , ityp), 'L', jaux)
        nnotyp(ityp) = zi(jaux)
        typgeo(ityp) = nummed(ityp)
!
 1  end do
!
    nbtyp = 0
    do 21 , ityp = 1 , ntymax
    if (nummed(ityp) .ne. 0) then
        do 211 , iaux = 1 , nbtyp
        if (nummed(ityp) .lt. nummed(renumd(iaux))) then
            jaux = iaux
            goto 212
        endif
211      continue
        jaux = nbtyp + 1
212      continue
        nbtyp = nbtyp + 1
        do 213 , iaux = nbtyp , jaux + 1 , -1
        renumd(iaux) = renumd(iaux-1)
213      continue
        renumd(jaux) = ityp
    endif
    21 end do
!
!====
! 3. CHANGEMENT DE CONVENTION DANS LES CONNECTIVITES ENTRE ASTER ET MED
!====
!
! 3.1. ==> PAR DEFAUT, LES DEUX NUMEROTATIONS SONT IDENTIQUES
!
    do 311 , iaux = 1 , ntymax
!
    modnum(iaux) = 0
!
    do 312 , jaux = 1 , nnomax
    nuanom(iaux,jaux) = 0
    numnoa(iaux,jaux) = 0
    312     end do
!
    311 end do
!
! 3.2. ==> MODIFICATIONS POUR LES TETRAEDRES
!       ------ TETRA4 -------
!
    modnum(18)=1
!
    nuanom(18,1)=1
    numnoa(18,1)=1
    nuanom(18,2)=3
    numnoa(18,2)=3
    nuanom(18,3)=2
    numnoa(18,3)=2
    nuanom(18,4)=4
    numnoa(18,4)=4
!
!       ------ TETRA10 -------
!
    modnum(19)=1
!
    nuanom(19,1)=1
    numnoa(19,1)=1
    nuanom(19,2)=3
    numnoa(19,2)=3
    nuanom(19,3)=2
    numnoa(19,3)=2
    nuanom(19,4)=4
    numnoa(19,4)=4
    nuanom(19,5)=7
    numnoa(19,5)=7
    nuanom(19,6)=6
    numnoa(19,6)=6
    nuanom(19,7)=5
    numnoa(19,7)=5
    nuanom(19,8)=8
    numnoa(19,8)=8
    nuanom(19,9)=10
    numnoa(19,9)=10
    nuanom(19,10)=9
    numnoa(19,10)=9
!
! 3.3. ==> MODIFICATIONS POUR LES PENTAEDRES
!       ------ PENTA6 -------
!
    modnum(20)=1
!
    nuanom(20,1)=1
    numnoa(20,1)=1
    nuanom(20,2)=3
    numnoa(20,2)=3
    nuanom(20,3)=2
    numnoa(20,3)=2
    nuanom(20,4)=4
    numnoa(20,4)=4
    nuanom(20,5)=6
    numnoa(20,5)=6
    nuanom(20,6)=5
    numnoa(20,6)=5
!
!       ------ PENTA15 -------
!
    modnum(21)=1
!
    nuanom(21,1)=1
    numnoa(21,1)=1
    nuanom(21,2)=3
    numnoa(21,2)=3
    nuanom(21,3)=2
    numnoa(21,3)=2
    nuanom(21,4)=4
    numnoa(21,4)=4
    nuanom(21,5)=6
    numnoa(21,5)=6
    nuanom(21,6)=5
    numnoa(21,6)=5
    nuanom(21,7)=9
    numnoa(21,7)=9
    nuanom(21,8)=8
    numnoa(21,8)=8
    nuanom(21,9)=7
    numnoa(21,9)=7
    nuanom(21,10)=15
    numnoa(21,10)=13
    nuanom(21,11)=14
    numnoa(21,11)=15
    nuanom(21,12)=13
    numnoa(21,12)=14
    nuanom(21,13)=10
    numnoa(21,13)=12
    nuanom(21,14)=12
    numnoa(21,14)=11
    nuanom(21,15)=11
    numnoa(21,15)=10
!
!
! 3.4. ==> MODIFICATIONS POUR LES PYRAMIDES
!       ------ PYRAM5 -------
!
    modnum(23)=1
!
    nuanom(23,1)=1
    numnoa(23,1)=1
    nuanom(23,2)=4
    numnoa(23,2)=4
    nuanom(23,3)=3
    numnoa(23,3)=3
    nuanom(23,4)=2
    numnoa(23,4)=2
    nuanom(23,5)=5
    numnoa(23,5)=5
!
!       ------ PYRAM13 -------
    modnum(24)=1
!
    nuanom(24,1)=1
    numnoa(24,1)=1
    nuanom(24,2)=4
    numnoa(24,2)=4
    nuanom(24,3)=3
    numnoa(24,3)=3
    nuanom(24,4)=2
    numnoa(24,4)=2
    nuanom(24,5)=5
    numnoa(24,5)=5
    nuanom(24,6)=9
    numnoa(24,6)=9
    nuanom(24,7)=8
    numnoa(24,7)=8
    nuanom(24,8)=7
    numnoa(24,8)=7
    nuanom(24,9)=6
    numnoa(24,9)=6
    nuanom(24,10)=10
    numnoa(24,10)=10
    nuanom(24,11)=13
    numnoa(24,11)=13
    nuanom(24,12)=12
    numnoa(24,12)=12
    nuanom(24,13)=11
    numnoa(24,13)=11
!
!
! 3.2. ==> MODIFICATIONS POUR LES HEXAEDRES
!
!       ------ HEXA8 -------
!
    modnum(25)=1
!
    nuanom(25,1)=1
    numnoa(25,1)=1
    nuanom(25,2)=4
    numnoa(25,2)=4
    nuanom(25,3)=3
    numnoa(25,3)=3
    nuanom(25,4)=2
    numnoa(25,4)=2
    nuanom(25,5)=5
    numnoa(25,5)=5
    nuanom(25,6)=8
    numnoa(25,6)=8
    nuanom(25,7)=7
    numnoa(25,7)=7
    nuanom(25,8)=6
    numnoa(25,8)=6
!
!       ------ HEXA20 -------
!
    modnum(26)=1
!
    nuanom(26,1)=1
    numnoa(26,1)=1
    nuanom(26,2)=4
    numnoa(26,2)=4
    nuanom(26,3)=3
    numnoa(26,3)=3
    nuanom(26,4)=2
    numnoa(26,4)=2
    nuanom(26,5)=5
    numnoa(26,5)=5
    nuanom(26,6)=8
    numnoa(26,6)=8
    nuanom(26,7)=7
    numnoa(26,7)=7
    nuanom(26,8)=6
    numnoa(26,8)=6
    nuanom(26,9)=12
    numnoa(26,9)=12
    nuanom(26,10)=11
    numnoa(26,10)=11
    nuanom(26,11)=10
    numnoa(26,11)=10
    nuanom(26,12)=9
    numnoa(26,12)=9
    nuanom(26,13)=20
    numnoa(26,13)=17
    nuanom(26,14)=19
    numnoa(26,14)=20
    nuanom(26,15)=18
    numnoa(26,15)=19
    nuanom(26,16)=17
    numnoa(26,16)=18
    nuanom(26,17)=13
    numnoa(26,17)=16
    nuanom(26,18)=16
    numnoa(26,18)=15
    nuanom(26,19)=15
    numnoa(26,19)=14
    nuanom(26,20)=14
    numnoa(26,20)=13
!
!       ------ HEXA27 -------
!
    modnum(27)=1
!
    nuanom(27,1)=1
    numnoa(27,1)=1
    nuanom(27,2)=4
    numnoa(27,2)=4
    nuanom(27,3)=3
    numnoa(27,3)=3
    nuanom(27,4)=2
    numnoa(27,4)=2
    nuanom(27,5)=5
    numnoa(27,5)=5
    nuanom(27,6)=8
    numnoa(27,6)=8
    nuanom(27,7)=7
    numnoa(27,7)=7
    nuanom(27,8)=6
    numnoa(27,8)=6
    nuanom(27,9)=12
    numnoa(27,9)=12
    nuanom(27,10)=11
    numnoa(27,10)=11
    nuanom(27,11)=10
    numnoa(27,11)=10
    nuanom(27,12)=9
    numnoa(27,12)=9
    nuanom(27,13)=20
    numnoa(27,13)=17
    nuanom(27,14)=19
    numnoa(27,14)=20
    nuanom(27,15)=18
    numnoa(27,15)=19
    nuanom(27,16)=17
    numnoa(27,16)=18
    nuanom(27,17)=13
    numnoa(27,17)=16
    nuanom(27,18)=16
    numnoa(27,18)=15
    nuanom(27,19)=15
    numnoa(27,19)=14
    nuanom(27,20)=14
    numnoa(27,20)=13
    nuanom(27,21)=21
    numnoa(27,21)=21
    nuanom(27,22)=25
    numnoa(27,22)=25
    nuanom(27,23)=24
    numnoa(27,23)=24
    nuanom(27,24)=23
    numnoa(27,24)=23
    nuanom(27,25)=22
    numnoa(27,25)=22
    nuanom(27,26)=26
    numnoa(27,26)=26
    nuanom(27,27)=27
    numnoa(27,27)=27
!
    call jedema()
!
end subroutine
