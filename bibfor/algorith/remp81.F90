subroutine remp81(nomres, lpar, basmod, nbmod)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
!
!  BUT : < REMPLISSAGE DES MACRO-ELEMENTS AVEC LES VALEURS GENERALISEES
!          RENSEIGNEES A LA MAIN >
!
!        LA MATRICE RESULTAT EST SYMETRIQUE ET STOCKEE TRIANGLE SUP
!
!-----------------------------------------------------------------------
!
! NOMRES /I/  : NOM K19 DE LA MATRICE CARREE RESULTAT
! LPAR   /I/  : ADRESSE POUR LE STOCKAGE DU PARAMETRE (MASSE, RAID...)
! BASMOD /K8/ : NOM UT DE LA BASE MODALE DE PROJECTION
! NBMOD  /I/  : NOMBRE DE MODES DANS LA BASE
!
!
!
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jexnum.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
    character(len=8) :: basmod, blanc
    character(len=18) :: nomres
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ldres, ldref, lddes, nbmod, imod
    integer ::  ntail, lpar, i, iad
!-----------------------------------------------------------------------
    data blanc/'        '/
!-----------------------------------------------------------------------
!
! --- CREATION ET REMPLISSAGE DU _REFE
!
    call jemarq()
    call wkvect(nomres//'_REFE', 'G V K24', 2, ldref)
    zk24(ldref) = basmod
    zk24(ldref+1) = blanc
!
! --- CREATION ET REMPLISSAGE DU _VALE
!
    ntail=nbmod*(nbmod+1)/2     
    call jecrec(nomres//'_VALE', 'G V R', 'NU', 'DISPERSE', & 
                   'CONSTANT',1)   
    call jeecra(nomres//'_VALE', 'LONMAX', ntail)
    call jecroc(jexnum(nomres//'_VALE', 1))
    call jeveuo(jexnum(nomres//'_VALE', 1), 'E', ldres)
!   call wkvect(nomres//'_VALE', 'G V R', ntail, ldres)
!
    do  i = 1, ntail
        zr(ldres+i-1)=0.0d0
    end do
    do  imod = 1, nbmod
        iad=imod*(imod+1)/2
!
        zr(ldres+iad-1)=zr(lpar+imod-1)
    end do
!
!
! --- CREATION ET REMPLISSAGE DU .DESC
!
    call wkvect(nomres(1:18)//'_DESC', 'G V I', 3, lddes)
    zi(lddes) = 2
    zi(lddes+1) = nbmod
    zi(lddes+2) = 2
!
    call jedema()
end subroutine
