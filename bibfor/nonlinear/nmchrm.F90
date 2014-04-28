subroutine nmchrm(phase, parmet, method, fonact, sddisc,&
                  sddyna, numins, iterat, defico, metpre,&
                  metcor, reasma)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisl.h"
#include "asterfort/diinst.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/ndynlo.h"
#include "asterfort/utmess.h"
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
    character(len=10), intent(in) :: phase
    real(kind=8), intent(in) :: parmet(*)
    character(len=16), intent(in) :: method(*)
    character(len=19), intent(in) :: sddisc
    character(len=19), intent(in) :: sddyna
    integer, intent(in) :: numins
    integer, intent(in) :: iterat
    character(len=24), intent(in) :: defico
    integer, intent(in) :: fonact(*)
    character(len=16), intent(out) :: metcor
    character(len=16), intent(out) :: metpre
    logical, intent(out) :: reasma
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! CHOIX DE REASSEMBLAGE DE LA MATRICE GLOBALE
!
! ----------------------------------------------------------------------
!
!
! IN  PHASE  : PHASE DE CALCUL
!                'PREDICTION'
!                'CORRECTION'
!                'FORCES_INT'
! IN  FONACT : FONCTIONNALITES ACTIVEES (vOIR NMFONC)
! IN  METHOD : INFORMATIONS SUR LES METHODES DE RESOLUTION
!                 VOIR DETAIL DES COMPOSANTES DANS NMLECT
! IN  PARMET : PARAMETRES DES METHODES DE RESOLUTION
!                 VOIR DETAIL DES COMPOSANTES DANS NMLECT
! IN  SDDISC : SD DISC_INST
! IN  SDDYNA : SD DYNAMIQUE
! IN  NUMINS : NUMERO D'INSTANT
! IN  ITERAT : NUMERO D'ITERATION
! IN  DEFICO : SD DEF. CONTACT
! OUT METCOR : TYPE DE MATRICE DE CORRECTION
! OUT METPRE : TYPE DE MATRICE DE PREDICTION
! OUT REASMA  : .TRUE. SI ASSEMBLAGE MATRICE GLOBALE
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    real(kind=8) :: instam, instap, pasmin, deltat
    integer :: reincr, reiter
    logical :: lmodim
    logical :: leltc, lctcd, lelas
    logical :: lprem, ldyna, lamor, lchoc, lvarc, l_elas_fo
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE><CALC> CHOIX D''ASSEMBLAGE DE '//&
        'MATRICE GLOBALE'
    endif
!
! --- INITIALISATIONS
!
    reasma = .false.
    lmodim = .false.
!
! --- PARAMETRES
!



    reincr = nint(parmet(1))
    reiter = nint(parmet(2))
    metcor = method(2)
    metpre = method(5)
    instam = diinst(sddisc, numins-1)
    instap = diinst(sddisc, numins )
    deltat = instap-instam
    pasmin = parmet(3)
!
! --- PREMIER PAS DE TEMPS ?
!
    lprem = numins.le.1
!
! --- FONCTIONNALITES ACTIVEES
!
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
    lamor = ndynlo(sddyna,'MAT_AMORT')
    lctcd = isfonc(fonact,'CONT_DISCRET')
    leltc = isfonc(fonact,'ELT_CONTACT')
    lchoc = isfonc(fonact,'DIS_CHOC')
    lvarc = isfonc(fonact,'EXI_VARC' )
    l_elas_fo = isfonc(fonact,'ELAS_FO' )
!
! --- AJOUTE-T-ON UNE CONTRIBUTION DU CONTACT DISCRET DANS LA MATRICE ?
!
    if (lctcd) then
        lmodim = cfdisl(defico,'MODI_MATR_GLOB')
    endif
!
! --- PASSAGE A LA MATRICE ELASTIQUE EN-DESSOUS DE PAS_MINI_ELAS
!
    if (abs(deltat) .lt. pasmin) then
        reincr = 1
        reiter = nint(parmet(4))
        metpre = 'SECANTE'
        metcor = 'SECANTE'
    endif
!
! --- REASSEMBLAGE DE LA MATRICE GLOBALE
!
    if (phase .eq. 'CORRECTION' .or. phase .eq. 'FORCES_INT') then
        if ((metcor.eq.'TANGENTE') .or. (metcor.eq.'SECANTE')) then
            reasma = .false.
            if (reiter .ne. 0) then
                reasma = mod(iterat+1,reiter) .eq. 0
            endif
        else
            reasma = .false.
        endif
    else if (phase.eq.'PREDICTION') then
        if ((reincr.eq.0) .and. (numins.ne.1)) then
            reasma = .false.
        endif
        if (numins .eq. 1) then
            reasma = .true.
        endif
        if ((reincr.ne.0) .and. (numins.ne.1)) then
            reasma = mod(numins-1,reincr) .eq. 0
        endif
    else
        ASSERT(.false.)
    endif
!
! --- ELASTICITE ?
!
    if ((metcor.eq.'ELASTIQUE') .or. (metpre.eq.'ELASTIQUE')) then
        lelas = .true.
    else
        lelas = .false.
    endif
!
! --- DYNAMIQUE: REACTUALISATION DE LA MATRICE
! --- D AMORTISSEMENT DE RAYLEIGH
!
    if (phase .eq. 'PREDICTION' .and. ldyna .and. lamor) then
        if (lprem) reasma = .true.
    endif
!
! --- SI ELEMENTS DE CONTACT XFEM/CONTACT_CONTINU - REASSEMBL. OBLIG.
!
    if (leltc) then
        if (.not.reasma) then
            if (.not.lelas) then
                call utmess('A', 'MECANONLINE5_4')
            endif
            reasma = .true.
        endif
    endif
!
! --- CONTACT DISCRET - CONTRIBUTION MATRICE TANGENTE
!
    if (lmodim .or. lchoc) then
        if (.not.reasma) then
            call utmess('A', 'MECANONLINE5_5')
            reasma = .true.
        endif
    endif
!
    if (lchoc) then
        metcor = 'TANGENTE'
        metpre = 'TANGENTE'
    endif
!
! --- VARIABLES COMMANDES: LA MATRICE ELASTIQUE DOIT ETRE REACTUALISEE
!
    if (lelas .and. phase .eq. 'PREDICTION') then
        if (lvarc .and. l_elas_fo) then
            reasma = .true.
        endif
    endif
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        if (reasma) then
            write (ifm,*) '<MECANONLINE><CALC> ON ASSEMBLE LA MATRICE'
        else
            write (ifm,*) '<MECANONLINE><CALC> ON N''ASSEMBLE PAS '//&
            'LA MATRICE'
        endif
    endif
!
    call jedema()
!
end subroutine
