subroutine cfgeom(reageo, iterat, noma, sdtime, sdstat,&
                  defico, resoco, depplu, instan)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/cfappa.h"
#include "asterfort/cfimp4.h"
#include "asterfort/geomco.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmrinc.h"
#include "asterfort/nmtime.h"
#include "asterfort/reajeu.h"
    integer :: iterat
    aster_logical :: reageo
    character(len=8) :: noma
    character(len=24) :: defico, resoco, sdtime, sdstat
    character(len=19) :: depplu
    real(kind=8) :: instan
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - APPARIEMENT)
!
! ROUTINE D'AIGUILLAGE POUR L'ACTUALISATION GEOMETRIQUE DU CONTACT:
!  APPARIEMENT, PROJECTION, JEUX
!
! ----------------------------------------------------------------------
!
!
! IN  REAGEO : VAUT .TRUE. SI REACTUALISATION GEOMETRIQUE
! IN  ITERAT : NUMERO DE L'ITERATION DE NEWTON COURANTE
! IN  NOMA   : NOM DU MAILLAGE
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  DEPPLU : CHAMP DE DEPLACEMENTS DEPUIS L'INSTANT INITIAL
!
!
!
!
    integer :: ifm, niv
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        if (reageo) then
            write (ifm,*) '<CONTACT> ... REACTUALISATION DE '//&
            'L''APPARIEMENT'
        else
            write (ifm,*) '<CONTACT> ... PAS DE REACTUALISATION DE '//&
            'L''APPARIEMENT'
        endif
    endif
!
! --- APPARIEMENT (OU PAS)
!
    if (reageo) then
        call nmtime(sdtime, 'INI', 'CONT_GEOM')
        call nmtime(sdtime, 'RUN', 'CONT_GEOM')
!
! ----- REACTUALISATION DE LA GEOMETRIE
!
        call geomco(noma, resoco, depplu)
!
! ----- APPARIEMENT
!
        call cfappa(noma, defico, resoco, instan)
        call nmtime(sdtime, 'END', 'CONT_GEOM')
        call nmrinc(sdstat, 'CONT_GEOM')
!
    else
!
! --- REACTUALISATION DU JEU NECESSAIRE EN DEBUT DE PAS
! --- DE TEMPS AU CAS OU IL Y AURAIT EU REDECOUPAGE
!
        if (iterat .eq. 0) then
            call reajeu(resoco)
        endif
    endif
!
! --- IMPRESSIONS POUR LES DEVELOPPEURS
!
    if (niv .ge. 2) then
        call cfimp4(defico, resoco, noma, ifm)
    endif
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        if (reageo) then
            write (ifm,*) '<CONTACT> ... FIN DE REACTUALISATION DE '//&
            'L''APPARIEMENT'
        endif
    endif
!
    call jedema()
end subroutine
