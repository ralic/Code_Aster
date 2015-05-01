subroutine excygl(nmresz, typsdz, mdcycz, maillz, profno)
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
    implicit none
!-----------------------------------------------------------------------
!  BUT:           < RESTITUTION CYCLIQUE GLOBALE >
!
!   RESTITUTION EN BASE PHYSIQUE DES RESULTATS CYCLIQUE
!    SUR UN MAILLAGE SQUELETTE DE LA STRUCTURE GLOBALE
!
! LE MAILLAGE REQUIS EST UN MAILLAGE AU SENS ASTER PLUS
! UN OBJET MAILLA//'.INV.SKELETON'
!
!-----------------------------------------------------------------------
!
! NMRESZ   /I/: NOM UT DU RESULTAT (TYPSD)
! MDCYCZ   /I/: NOM UT DU MODE_CYCL AMONT
! MAILLA   /I/: NOM UT DU MAILLAGE SQUELETTE SUPPORT
! PROFNO   /I/: NOM K19 DU PROFNO  DU SQUELETTE
! TYPSDZ   /I/: TYPE STRUCTURE DONNE RESULTAT (MODE_MECA,BASE_MODALE)
!
!
!
!
#include "jeveux.h"
#include "asterfort/cynupl.h"
#include "asterfort/exphgl.h"
#include "asterfort/getvis.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsutnu.h"
#include "asterfort/titre.h"
#include "asterfort/utmess.h"
    character(len=6) :: pgc
    character(len=*) :: nmresz, mdcycz, typsdz, maillz
    character(len=8) :: nomres, mailla, modcyc
    character(len=16) :: typsd
    character(len=19) :: profno
    character(len=24) :: indirf
    integer :: numdia, nbsec
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ibid, iret, nbmcal
!-----------------------------------------------------------------------
    data pgc /'EXCYGL'/
!-----------------------------------------------------------------------
!
    call jemarq()
    nomres = nmresz
    modcyc = mdcycz
    typsd = typsdz
    mailla = maillz
!
    indirf='&&'//pgc//'.INDIR.SECT'
!
!-----------------ECRITURE DU TITRE-------------------------------------
!
    call titre()
!
!--------------VERIFICATION SUR MAILLAGE SQUELETTE----------------------
!
    call jeexin(mailla//'.INV.SKELETON', iret)
    if (iret .eq. 0) then
        call utmess('F', 'ALGORITH13_8')
    endif
!
!-----RECUPERATION DU NOMBRE DE SECTEURS--------------------------------
    call getvis('CYCLIQUE', 'NB_SECTEUR', iocc=1, scal=nbsec, nbret=ibid)
!
    call getvis('CYCLIQUE', 'NUME_DIAMETRE', iocc=1, scal=numdia, nbret=ibid)
!
!-----RECUPERATION NOMBRE NUMERO D'ORDRE UTILISES POUR CALCUL CYCLIQUE--
    call rsutnu(modcyc, ' ', 0, '&&EXCYGL.NUME', nbmcal,&
                0.d0, 'ABSO', iret)
!
!--------------ALLOCATION DU CONCEPT MODE_MECA RESULTAT-----------------
!
    call rscrsd('G', nomres, typsd, nbmcal)
!
!-------------------CREATION PROF_CHAMNO ET TABLES INDIRECTION----------
!
    call cynupl(profno, indirf, modcyc, mailla, nbsec)
!
!------------------------------RESTITUTION -----------------------------
!
    call exphgl(nomres, typsd, modcyc, profno, indirf,&
                mailla, nbsec, numdia, nbmcal)
!
    call jedetr('&&'//pgc//'.INDIR.SECT')
    call jedema()
end subroutine
