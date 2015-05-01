subroutine recygl(nmresz, typsdz, mdcycz, maillz, profno)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!***********************************************************************
!    P. RICHARD     DATE 10/02/92
!-----------------------------------------------------------------------
!  BUT:           < RESTITUTION CYCLIQUE GLOBALE >
    implicit none
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
#include "asterfort/cynugl.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/recbgl.h"
#include "asterfort/remngl.h"
#include "asterfort/rscrsd.h"
#include "asterfort/titre.h"
#include "asterfort/utmess.h"
    character(len=6) :: pgc
    character(len=*) :: nmresz, mdcycz, typsdz, maillz
    character(len=8) :: nomres, mailla, modcyc, typint
    character(len=16) :: typsd
    character(len=19) :: profno
    character(len=24) :: indirf
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, icomp, idia, iret
    integer :: llref,  mdiapa, nbdia, nbmcal, nbmor, nbsec
    integer, pointer :: cycl_desc(:) => null()
    integer, pointer :: cycl_nbsc(:) => null()
    integer, pointer :: cycl_diam(:) => null()
    character(len=8), pointer :: cycl_type(:) => null()
!
!-----------------------------------------------------------------------
    data pgc /'RECYGL'/
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
!
!-------------------RECUPERATION DE LA BASE MODALE----------------------
!
    call jeveuo(modcyc//'.CYCL_REFE', 'L', llref)
!
!-----------------------RECUPERATION DU TYPE INTERFACE------------------
!
!
    call jeveuo(modcyc//'.CYCL_TYPE', 'L', vk8=cycl_type)
    typint=cycl_type(1)
!
!
!------------------RECUPERATION DU NOMBRE DE SECTEURS-------------------
!
!
    call jeveuo(modcyc//'.CYCL_NBSC', 'L', vi=cycl_nbsc)
    nbsec=cycl_nbsc(1)
    mdiapa=int(nbsec/2)*int(1-nbsec+(2*int(nbsec/2)))
!
!-----RECUPERATION NOMBRE MODES PROPRES UTILISES POUR CALCUL CYCLIQUE---
!            ET NOMBRE DE MODES CALCULES PAR DIAMETRE NODAUX
!
    call jeveuo(modcyc//'.CYCL_DESC', 'L', vi=cycl_desc)
    nbmcal=cycl_desc(4)
!
!----------DETERMINATION DU NOMBRE DE MODES PHYSIQUE A RESTITUER--------
!
    call jeveuo(modcyc//'.CYCL_DIAM', 'L', vi=cycl_diam)
    call jelira(modcyc//'.CYCL_DIAM', 'LONMAX', nbdia)
    nbdia=nbdia/2
!
    icomp=0
    do 10 i = 1, nbdia
        idia=cycl_diam(i)
        nbmcal=cycl_diam(1+nbdia+i-1)
        if (idia .eq. 0 .or. idia .eq. mdiapa) then
            icomp=icomp+nbmcal
        else
            icomp=icomp+2*nbmcal
        endif
10  end do
!
    nbmor=icomp
!
!--------------ALLOCATION DU CONCEPT MODE_MECA RESULTAT-----------------
!
    write(6,*)'RECYGL NOMRES: ',nomres
    write(6,*)'RECYGL TYPSD: ',typsd
    write(6,*)'RECYGL NBMOR: ',nbmor
    call rscrsd('G', nomres, typsd, nbmor)
!
!-------------------CREATION PROF_CHAMNO ET TABLES INDIRECTION----------
!
    call cynugl(profno, indirf, modcyc, mailla)
!
!------------------------------RESTITUTION -----------------------------
!
!
!
!    CAS CRAIG-BAMPTON ET CRAIG-BAMPTON HARMONIQUE
!
    if (typint .eq. 'CRAIGB  ' .or. typint .eq. 'CB_HARMO') then
        call recbgl(nomres, typsd, modcyc, profno, indirf,&
                    mailla)
    endif
!
!
!    CAS MAC NEAL AVEC ET SANS CORRECTION
!
    if (typint .eq. 'MNEAL   ' .or. typint .eq. 'AUCUN   ') then
        call remngl(nomres, typsd, modcyc, profno, indirf,&
                    mailla)
    endif
!
    call jedetr('&&'//pgc//'.INDIR.SECT')
    call jedema()
end subroutine
