subroutine nmcore(sdcrit, sderro, sdconv, defico, numins,&
                  iterat, fonact, relite, eta, parcri,&
                  vresi, vrela, vmaxi, vchar, vrefe,&
                  vcomp, vfrot, vgeom)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterc/r8vide.h"
#include "asterfort/cfdisr.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmcoru.h"
#include "asterfort/nmcrel.h"
#include "asterfort/nmevcv.h"
#include "asterfort/nmlecv.h"
#include "asterfort/u2mesr.h"
#include "asterfort/u2mess.h"
    integer :: fonact(*)
    real(kind=8) :: parcri(*)
    integer :: numins, iterat, relite
    character(len=24) :: sderro, sdconv, defico
    character(len=19) :: sdcrit
    real(kind=8) :: eta
    real(kind=8) :: vresi, vrela, vmaxi, vchar, vrefe, vcomp, vfrot, vgeom
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - CONVERGENCE)
!
! VERIFICATION DES CRITERES D'ARRET SUR RESIDUS
!
! ----------------------------------------------------------------------
!
!
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  SDCONV : SD GESTION DE LA CONVERGENCE
! IN  SDERRO : GESTION DES ERREURS
! IN  SDCRIT : SYNTHESE DES RESULTATS DE CONVERGENCE POUR ARCHIVAGE
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  NUMINS : NUMERO DU PAS DE TEMPS
! IN  ITERAT : NUMERO D'ITERATION
! IN  ETA    : COEFFICIENT DE PILOTAGE
! IN  RELITE : NOMBRE D'ITERATIONS DE RECHERCHE LINEAIRE
! IN  PARCRI : CRITERES DE CONVERGENCE (VOIR NMDOCN)
! IN  VRESI  : RESIDU D'EQUILIBRE
! IN  VRELA  : RESI_GLOB_RELA MAXI
! IN  VMAXI  : RESI_GLOB_MAXI MAXI
! IN  VCOMP  : RESI_COMP_RELA MAXI
! IN  VCHAR  : CHARGEMENT EXTERIEUR MAXI
! IN  VREFE  : RESI_GLOB_REFE MAXI
! IN  VFROT  : RESI_FROT MAXI
! IN  VGEOM  : RESI_GEOM MAXI
!
! ----------------------------------------------------------------------
!
    integer :: nresi
    parameter    (nresi=6)
!
    character(len=24) :: cnvact
    integer :: jcnvac
    character(len=24) :: critcr
    integer :: jcrr
    real(kind=8) :: valr(2), detect
    real(kind=8) :: chmini
    integer :: iresi
    logical :: convok(nresi), convno(nresi)
    real(kind=8) :: resi(nresi), resid(nresi)
    logical :: lrela, lmaxi, lrefe, lcomp, lfrot, lgeom
    logical :: cvresi, maxrel, maxnod
    logical :: lcont
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD CONVERGENCE
!
    cnvact = sdconv(1:19)//'.ACTI'
    call jeveuo(cnvact, 'E', jcnvac)
    critcr = sdcrit(1:19)//'.CRTR'
    call jeveuo(critcr, 'E', jcrr)
!
! --- INITIALISATIONS
!
    lcont = isfonc(fonact,'CONTACT')
    cvresi = .true.
    maxrel = .false.
    maxnod = .false.
    chmini = zr(jcrr+6-1)
!
! --- RESIDUS A TESTER POUR LE CRITERE D'ARRET
!
    lrela = (parcri(2) .ne. r8vide())
    lmaxi = (parcri(3) .ne. r8vide())
    lrefe = (parcri(6) .ne. r8vide())
    lcomp = (parcri(7) .ne. r8vide())
    lfrot = zl(jcnvac-1+5)
    lgeom = zl(jcnvac-1+6)
!
! --- VALEURS CALCULEES ET REFERENCES
!
    resi(1) = vrela
    resi(2) = vmaxi
    resi(3) = vrefe
    resi(4) = vcomp
    resi(5) = vfrot
    resi(6) = vgeom
    resid(1) = parcri(2)
    resid(2) = parcri(3)
    resid(3) = parcri(6)
    resid(4) = parcri(7)
    if (lcont) resid(5) = cfdisr(defico,'RESI_FROT')
    if (lcont) resid(6) = cfdisr(defico,'RESI_GEOM')
    call nmcrel(sderro, 'DIVE_RELA', .false.)
    call nmcrel(sderro, 'DIVE_MAXI', .false.)
    call nmcrel(sderro, 'DIVE_REFE', .false.)
    call nmcrel(sderro, 'DIVE_COMP', .false.)
    call nmcrel(sderro, 'DIVE_FROT', .false.)
    call nmcrel(sderro, 'DIVE_GEOM', .false.)
!
! --- SI CRITERE RESI_COMP_RELA ET PREMIER INSTANT
! --- -> ON UTILISE RESI_GLOB_RELA
!
    if (lcomp) then
        if (numins .eq. 1) then
            lrela = .true.
            lcomp = .false.
            maxnod = .true.
            resid(1) = resid(4)
            resid(2) = resid(4)
            call u2mess('I', 'MECANONLINE2_96')
        endif
    endif
!
! --- SI CRITERE RESI_GLOB_RELA ET CHARGEMENT NUL
! --- -> ON UTILISE RESI_GLOB_MAXI
!
    if (lrela) then
        detect = 1.d-6 * chmini
        if (vchar .le. detect) then
            if (numins .gt. 1) then
                lrela = .false.
                lmaxi = .true.
                maxrel = .true.
                resid(2) = zr(jcrr+7-1)
                valr(1) = detect
                valr(2) = resid(2)
                call u2mesr('I', 'MECANONLINE2_98', 2, valr)
            endif
            if ((parcri(7) .ne. r8vide()) .and. numins .eq. 1) then
                lrela = .false.
                lmaxi = .true.
                maxrel = .true.
                valr(1) = detect
                valr(2) = resid(2)
                call u2mesr('I', 'MECANONLINE2_98', 2, valr)
            endif
        endif
    endif
!
! --- NOUVEAUX CRITERES APRES BASCULEMENT(S) EVENTUEL(S)
!
    zl(jcnvac-1+1) = lrela
    zl(jcnvac-1+2) = lmaxi
    zl(jcnvac-1+3) = lrefe
    zl(jcnvac-1+4) = lcomp
    zl(jcnvac-1+5) = lfrot
    zl(jcnvac-1+6) = lgeom
!
! --- CRITERES D'ARRET
!
    do 10 iresi = 1, nresi
        if (zl(jcnvac+iresi-1)) then
            call nmcoru(resi(iresi), resid(iresi), convok(iresi))
        else
            convok(iresi) = .true.
        endif
10  end do
!
! --- ENREGISTREMENT DES EVENEMENTS
!
    convno(1) = .not.convok(1)
    convno(2) = .not.convok(2)
    convno(3) = .not.convok(3)
    convno(4) = .not.convok(4)
    convno(5) = .not.convok(5)
    convno(6) = .not.convok(6)
    if (lrela) call nmcrel(sderro, 'DIVE_RELA', convno(1))
    if (lmaxi) call nmcrel(sderro, 'DIVE_MAXI', convno(2))
    if (lrefe) call nmcrel(sderro, 'DIVE_REFE', convno(3))
    if (lcomp) call nmcrel(sderro, 'DIVE_COMP', convno(4))
    if (lfrot) call nmcrel(sderro, 'DIVE_FROT', convno(5))
    if (lgeom) call nmcrel(sderro, 'DIVE_GEOM', convno(6))
    call nmcrel(sderro, 'RESI_MAXR', maxrel)
    call nmcrel(sderro, 'RESI_MAXN', maxnod)
!
! --- EVALUATION DE LA CONVERGENCE DU RESIDU
!
    call nmevcv(sderro, fonact, 'RESI')
    call nmlecv(sderro, 'RESI', cvresi)
!
! --- SAUVEGARDES INFOS CONVERGENCE
!
    zr(jcrr+1-1) = iterat+1
    zr(jcrr+2-1) = relite
    zr(jcrr+3-1) = vrela
    zr(jcrr+4-1) = vmaxi
    zr(jcrr+5-1) = eta
    if ((numins.eq.1) .and. (iterat.eq.0)) then
        zr(jcrr+6-1) = vchar
    else
        if (cvresi .and. (.not.maxrel)) then
            zr(jcrr+6-1) = min(vchar, zr(jcrr+6-1))
        endif
    endif
    if (cvresi) then
        zr(jcrr+7-1) = vresi
    endif
    zr(jcrr+8-1) = vrefe
    zr(jcrr+9-1) = vcomp
!
    call jedema()
end subroutine
