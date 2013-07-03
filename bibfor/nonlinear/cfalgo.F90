subroutine cfalgo(noma, sdstat, resigr, iterat, defico,&
                  resoco, solveu, numedd, matass, ddepla,&
                  depdel, ctccvg, ctcfix)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
#include "jeveux.h"
#include "asterfort/algocg.h"
#include "asterfort/algocl.h"
#include "asterfort/algoco.h"
#include "asterfort/algocp.h"
#include "asterfort/algogl.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfeven.h"
#include "asterfort/cfpost.h"
#include "asterfort/cfprep.h"
#include "asterfort/fro2gd.h"
#include "asterfort/frogdp.h"
#include "asterfort/frolgd.h"
#include "asterfort/fropgd.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    logical :: ctcfix
    character(len=8) :: noma
    real(kind=8) :: resigr
    integer :: iterat
    character(len=24) :: sdstat
    character(len=24) :: defico, resoco
    character(len=19) :: ddepla, depdel
    character(len=19) :: solveu, matass
    character(len=14) :: numedd
    integer :: ctccvg
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - APPARIEMENT)
!
! ROUTINE D'AIGUILLAGE POUR LA RESOLUTION DU CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  SDSTAT : SD STATISTIQUES
! IN  NOMA   : NOM DU MAILLAGE
! IN  RESIGR : RESI_GLOB_RELA
! IN  ITERAT : ITERATION DE NEWTON
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE RESOLUTION DU CONTACT
! IN  SOLVEU : SD SOLVEUR
! IN  NUMEDD : NUME_DDL
! IN  MATASS : NOM DE LA MATRICE DU PREMIER MEMBRE ASSEMBLEE
! IN  DDEPLA : INCREMENT DE DEPLACEMENTS CALCULE EN IGNORANT LE CONTACT
! IN  DEPDEL : INCREMENT DE DEPLACEMENT CUMULE DEPUIS DEBUT DU PAS
! OUT CTCFIX : .TRUE.  SI ATTENTE POINT FIXE CONTACT
! OUT CTCCVG : CODE RETOUR CONTACT DISCRET
!                -1 : PAS DE CALCUL DU CONTACT DISCRET
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : NOMBRE MAXI D'ITERATIONS
!                 2 : MATRICE SINGULIERE
!
!
!
!
    integer :: ifm, niv
    integer :: icont, ifrot, ndimg
    logical :: lgliss
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ... DEBUT DE LA RESOLUTION DU '//&
        'CONTACT'
    endif
!
! --- METHODE DE CONTACT ET DE FROTTEMENT
!
    icont = cfdisi(defico,'ALGO_CONT')
    ifrot = cfdisi(defico,'ALGO_FROT')
    lgliss = cfdisl(defico,'CONT_DISC_GLIS')
    ndimg = cfdisi(defico,'NDIM' )
!
! --- INITIALISATIONS
!
    ctccvg = 0
    ctcfix = .false.
!
! --- PREPARATION DES CALCULS
!
    call cfprep(noma, defico, resoco, matass, ddepla,&
                depdel)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ...... DEBUT DE REALISATION DU CALCUL'
    endif
!
! --- CHOIX DE L'ALGO DE CONTACT/FROTTEMENT
!
    if (icont .eq. 4) then
        if (ifrot .eq. 0) then
            call algocp(sdstat, resoco, numedd, matass)
        else if (ifrot.eq.1) then
            call frogdp(sdstat, resoco, numedd, matass, resigr)
!
        else
            call assert(.false.)
        endif
    else if (icont.eq.1) then
        if (lgliss) then
            call algogl(sdstat, defico, resoco, solveu, matass,&
                        noma, ctccvg)
        else
            call algoco(sdstat, defico, resoco, solveu, matass,&
                        noma, ctccvg)
        endif
    else if (icont.eq.2) then
        if (ifrot .eq. 0) then
            call algocg(sdstat, defico, resoco, solveu, matass,&
                        ctccvg)
        else
            call assert(.false.)
        endif
!
    else if (icont.eq.5) then
        if (ifrot .eq. 0) then
            call algocl(sdstat, defico, resoco, solveu, matass,&
                        noma, ctccvg, ctcfix)
        else if (ifrot.eq.1) then
            call fropgd(sdstat, defico, resoco, solveu, numedd,&
                        matass, noma, resigr, depdel, ctccvg,&
                        ctcfix)
        else if (ifrot.eq.2) then
            if (ndimg .eq. 2) then
                call fro2gd(sdstat, defico, resoco, solveu, matass,&
                            noma, ctccvg)
            else if (ndimg.eq.3) then
                call frolgd(sdstat, defico, resoco, solveu, numedd,&
                            matass, noma, resigr, depdel, ctccvg)
            else
                call assert(.false.)
            endif
        else
            call assert(.false.)
        endif
    else
        call assert(.false.)
    endif
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ...... FIN DE REALISATION DU CALCUL'
    endif
!
! --- POST-TRAITEMENTS DES CALCULS
!
    call cfpost(noma, defico, resoco, ddepla, ctccvg)
!
! --- ETAT POUR EVENT-DRIVEN
!
    if (iterat .eq. 0) call cfeven('INI', defico, resoco)
    call cfeven('FIN', defico, resoco)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ... FIN DE LA RESOLUTION DU '//&
        'CONTACT'
    endif
!
! --- LE CALCUL DE CONTACT A FORCEMENT ETE REALISE
!
    call assert(ctccvg.ge.0)
!
    call jedema()
end subroutine
