subroutine nmresi(noma, mate, numedd, sdnume, fonact,&
                  sddyna, sdconv, sdimpr, defico, resoco,&
                  matass, numins, conv, resigr, eta, &
                  comref, valinc, solalg, veasse, measse,&
                  vrela, vmaxi, vchar, vresi, vrefe, &
                  vinit, vcomp, vfrot, vgeom)
!
    implicit     none
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/dismoi.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mmconv.h"
#include "asterfort/ndiner.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmequi.h"
#include "asterfort/nmigno.h"
#include "asterfort/nmimre.h"
#include "asterfort/nmpcin.h"
#include "asterfort/nmrede.h"
#include "asterfort/nmvcmx.h"
#include "asterfort/rescmp.h"
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
! aslint: disable=W1504
!
    character(len=8) :: noma
    character(len=24) :: numedd
    character(len=24) :: defico, resoco
    character(len=24) :: sdimpr, sdconv, mate
    integer :: numins
    character(len=19) :: sddyna, sdnume
    character(len=19) :: measse(*), veasse(*)
    character(len=19) :: valinc(*), solalg(*)
    character(len=19) :: matass
    character(len=24) :: comref
    integer :: fonact(*)
    real(kind=8) :: eta, conv(*), resigr
    real(kind=8) :: vrela, vmaxi, vchar, vresi, vrefe, vinit, vcomp, vfrot
    real(kind=8) :: vgeom
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE)
!
! CALCULS DES RESIDUS D'EQUILIBRE ET DES CHARGEMENTS POUR
! ESTIMATION DE LA CONVERGENCE
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  SDIMPR : SD AFFICHAGE
! IN  NUMEDD : NUMEROTATION NUME_DDL
! IN  SDNUME : NOM DE LA SD NUMEROTATION
! IN  SDCONV : SD GESTION DE LA CONVERGENCE
! IN  COMREF : VARI_COM REFE
! IN  MATASS : MATRICE DU PREMIER MEMBRE ASSEMBLEE
! IN  NUMINS : NUMERO D'INSTANT
! IN  RESIGR : RESI_GLOB_RELA
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  MEASSE : VARIABLE CHAPEAU POUR NOM DES MATR_ASSE
! IN  ETA    : COEFFICIENT DE PILOTAGE
! OUT CONV   : INFORMATIONS SUR LA CONVERGENCE DU CALCUL
!                 3 - RESI_GLOB_RELA
!                 4 - RESI_GLOB_MAXI
! OUT VRELA  : RESI_GLOB_RELA MAXI
! OUT VMAXI  : RESI_GLOB_MAXI MAXI
! OUT VCHAR  : CHARGEMENT EXTERIEUR MAXI
! OUT VRESI  : RESIDU EQUILIBRE MAXI
! OUT VREFE  : RESI_GLOB_REFE MAXI
! OUT VINIT  : CONTRAINTES INITIALES MAXI
! OUT VCOMP  : RESI_COMP_RELA MAXI
! OUT VFROT  : RESI_FROT MAXI
!
! ----------------------------------------------------------------------
!
    integer :: jccid, jfint, jdiri, jfext, jvcfo, jrefe, jiner, jvcf1
    integer :: ifm, niv, nocc
    integer :: neq
    character(len=8) :: k8bid, noddlm
    logical :: ldyna, lstat, lcine, lctcc
    character(len=19) :: profch, foiner
    character(len=19) :: commoi, depmoi
    character(len=19) :: cndiri, cnbudi, cnvcfo, cnfext, cnvcf1, cnrefe, cnfint
    character(len=19) :: cnfnod, cndipi, cndfdo
    integer :: jdeeq, jfnod, jbudi, jdfdo, jdipi
    integer :: ibid, ier, ieq, iret
    logical :: lrefe, linit, lcmp
    real(kind=8) :: val1, val4, val5
    real(kind=8) :: maxres
    integer :: irela, imaxi, iresi, irefe, ichar, icomp
    logical :: lndepl, lpilo
    character(len=16) :: nfrot, ngeom
    character(len=24) :: sdnuco
    integer :: jnuco
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... CALCUL DES RESIDUS'
    endif
!
! --- INITIALISATIONS
!
    vrela = 0.d0
    vmaxi = 0.d0
    vrefe = 0.d0
    vchar = 0.d0
    vresi = 0.d0
    vcomp = 0.d0
    vinit = 0.d0
    vfrot = 0.d0
    vgeom = 0.d0
    irela = 0
    imaxi = 0
    irefe = 0
    iresi = 0
    ichar = 0
    icomp = 0
    jccid = 0
    call dismoi('F', 'NB_EQUA', numedd, 'NUME_DDL', neq,&
                k8bid, iret)
!
! --- FONCTIONNALITES ACTIVEES
!
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
    lstat = ndynlo(sddyna,'STATIQUE')
    lrefe = isfonc(fonact,'RESI_REFE')
    lcmp = isfonc(fonact,'RESI_COMP')
    lpilo = isfonc(fonact,'PILOTAGE')
    lcine = isfonc(fonact,'DIRI_CINE')
    lctcc = isfonc(fonact,'CONT_CONTINU')
    call getfac('ETAT_INIT', nocc)
    linit = (numins.eq.1).and.(nocc.eq.0)
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPMOI', depmoi)
    call nmchex(valinc, 'VALINC', 'COMMOI', commoi)
    call nmchex(veasse, 'VEASSE', 'CNDIRI', cndiri)
    call nmchex(veasse, 'VEASSE', 'CNBUDI', cnbudi)
    call nmchex(veasse, 'VEASSE', 'CNVCF0', cnvcfo)
    call nmchex(veasse, 'VEASSE', 'CNVCF1', cnvcf1)
    call nmchex(veasse, 'VEASSE', 'CNREFE', cnrefe)
    call nmchex(veasse, 'VEASSE', 'CNFNOD', cnfnod)
    call nmchex(veasse, 'VEASSE', 'CNDIPI', cndipi)
    cndfdo = '&&CNCHAR.DFDO'
!
! --- CALCUL DE LA FORCE DE REFERENCE POUR LA DYNAMIQUE
!
    if (ldyna) then
        foiner = '&&CNPART.CHP1'
        call ndiner(numedd, sddyna, valinc, measse, foiner)
    endif
!
! --- TYPE DE FORMULATION
!
    lndepl = .not.(ndynlo(sddyna,'FORMUL_DEPL').or.lstat)
!
! --- RESULTANTE DES EFFORTS POUR ESTIMATION DE L'EQUILIBRE
!
    call nmequi(eta, fonact, sddyna, foiner, veasse,&
                cnfext, cnfint)
!
! --- POINTEUR SUR LES DDLS ELIMINES PAR AFFE_CHAR_CINE
!
    if (lcine) then
        call nmpcin(matass)
        call jeveuo(matass(1:19)//'.CCID', 'L', jccid)
    endif
!
! --- REPERAGE DDL LAGRANGE DE CONTACT
!
    sdnuco = sdnume(1:19)//'.NUCO'
    call jeveuo(sdnuco, 'L', jnuco)
!
! --- ACCES NUMEROTATION DUALISATION DES EQUATIONS
!
    call dismoi('F', 'PROF_CHNO', depmoi, 'CHAM_NO', ibid,&
                profch, ier)
    call jeveuo(profch(1:19)//'.DEEQ', 'L', jdeeq)
!
!
! --- CALCULE LE MAX DES RESIDUS PAR CMP POUR LE RESIDU RESI_COMP_RELA
!
    if (lcmp) then
        call rescmp(cndiri, cnvcfo, cnfext, cnfint, cnfnod,&
                    maxres, noddlm, icomp)
    endif
!
! --- ACCES AUX CHAM_NO
!
    call jeveuo(cnfint(1:19)//'.VALE', 'L', jfint)
    call jeveuo(cndiri(1:19)//'.VALE', 'L', jdiri)
    call jeveuo(cnfext(1:19)//'.VALE', 'L', jfext)
    call jeveuo(cnvcfo(1:19)//'.VALE', 'L', jvcfo)
    call jeveuo(cnbudi(1:19)//'.VALE', 'L', jbudi)
    call jeveuo(cndfdo(1:19)//'.VALE', 'L', jdfdo)
    if (lpilo) then
        call jeveuo(cndipi(1:19)//'.VALE', 'L', jdipi)
    endif
    if (ldyna) then
        call jeveuo(foiner(1:19)//'.VALE', 'L', jiner)
    endif
    if (linit) then
        call jeveuo(cnvcf1(1:19)//'.VALE', 'L', jvcf1)
    endif
    if (lrefe) then
        call jeveuo(cnrefe(1:19)//'.VALE', 'L', jrefe)
    endif
    if (lcine) then
        call jeveuo(cnfnod(1:19)//'.VALE', 'L', jfnod)
    endif
!
! --- CALCUL DES FORCES POUR MISE A L'ECHELLE (DENOMINATEUR)
!
    call nmrede(numedd, sdnume, fonact, sddyna, matass,&
                veasse, neq, foiner, cnfext, cnfint,&
                vchar, ichar)
!
! --- CALCUL DES RESIDUS
!
    do ieq = 1, neq
!
! ----- SI SCHEMA NON EN DEPLACEMENT: ON IGNORE LA VALEUR DU RESIDU
!
        if (nmigno(jdiri ,lndepl,ieq)) then
            goto 20
        endif
!
! ----- SI CHARGEMENT CINEMATIQUE: ON IGNORE LA VALEUR DU RESIDU
!
        if (lcine) then
            if (zi(jccid+ieq-1) .eq. 1) then
                goto 20
            endif
        endif
!
! ----- SI LAGRANGIEN DE CONTACT/FROT: ON IGNORE LA VALEUR DU RESIDU
!
        if (zi(jnuco+ieq-1) .eq. 1) then
            goto 20
        endif
!
! --- CALCUL DU RESIDU A PROPREMENT PARLER
!
        if (lpilo) then
            val1 = abs(&
                   zr(jfint+ieq-1)+zr(jdiri+ieq-1)+zr(jbudi+ieq-1) -zr(jfext+ieq-1)-zr(jdfdo+ieq-&
                   &1)-eta*zr(jdipi+ieq-1)&
                   )
        else
            val1 = abs(&
                   zr(jfint+ieq-1)+zr(jdiri+ieq-1)+zr(jbudi+ieq-1) -zr(jfext+ieq-1)-zr(jdfdo+ieq-&
                   &1)&
                   )
        endif
!
! --- VRESI: MAX RESIDU D'EQUILIBRE
!
        if (vresi .le. val1) then
            vresi = val1
            iresi = ieq
        endif
!
! --- SI CONVERGENCE EN CONTRAINTE ACTIVE
!
        if (lrefe) then
            if (zi(jdeeq-1 + 2*ieq) .gt. 0) then
                val4 = abs(&
                       zr(jfint+ieq-1)+zr(jdiri+ieq-1)+zr(jbudi+ ieq-1) -zr(jfext+ieq-1)-zr(jdfdo&
                       &+ieq-1))/zr(jrefe+ieq- 1&
                       )
                if (vrefe .le. val4) then
                    vrefe = val4
                    irefe = ieq
                endif
            endif
        endif
!
! --- SI TEST CONTRAINTES INITIALES
!
        if (linit) then
            val5 = abs(zr(jvcf1+ieq-1))
            if (vinit .le. val5) then
                vinit = val5
            endif
        endif
!
 20     continue
    end do
!
! --- SYNTHESE DES RESULTATS
!
    vmaxi = vresi
    imaxi = iresi
    if (vchar .gt. 0.d0) then
        vrela = vresi/vchar
        irela = iresi
    else
        vrela = -1.d0
    endif
!
    if (lcmp) then
        vcomp = maxres
    endif
!
! --- RESIDUS SPECIFIQUES POUR NEWTON GENERALISE
!
    if (lctcc) then
        call mmconv(noma, defico, resoco, valinc, solalg,&
                    vfrot, nfrot, vgeom, ngeom)
    endif
!
! --- ECRITURE DES INFOS SUR LES RESIDUS POUR AFFICHAGE
!
    call nmimre(numedd, sdimpr, sdconv, vrela, vmaxi,&
                vrefe, vcomp, vfrot, vgeom, irela,&
                imaxi, irefe, noddlm, icomp, nfrot,&
                ngeom)
!
! --- SAUVEGARDES RESIDUS
!
    conv(3) = vrela
    conv(4) = vmaxi
!
! --- VERIFICATION QUE LES VARIABLES DE COMMANDE INITIALES CONDUISENT
! --- A DES FORCES NODALES NULLES
!
    if (linit) then
        if (vchar .gt. resigr) then
            vinit = vinit/vchar
        endif
        if (vinit .gt. resigr) then
            call nmvcmx(mate, noma, comref, commoi)
        endif
    endif
!
    call jedema()
end subroutine
