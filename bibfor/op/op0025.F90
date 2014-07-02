subroutine op0025()
!
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     COMMANDE:  THER_LINEAIRE
! ----------------------------------------------------------------------
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
!
! 0.2. ==> COMMUNS
!
!
!
!
! 0.3. ==> VARIABLES LOCALES
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/etausr.h"
#include "asterfort/assert.h"
#include "asterfort/detmat.h"
#include "asterfort/didern.h"
#include "asterfort/diinst.h"
#include "asterfort/exixfe.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/medith.h"
#include "asterfort/ntarch.h"
#include "asterfort/ntinit.h"
#include "asterfort/ntreso.h"
#include "asterfort/nxlect.h"
#include "asterfort/sigusr.h"
#include "asterfort/titre.h"
#include "asterfort/utmess.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
#include "asterfort/vtcreb.h"
#include "asterfort/xthpos.h"
    character(len=6) :: nompro
    parameter ( nompro = 'OP0025' )
!
    integer :: vali
    integer :: ifm, niv, iret
    integer :: neq, numins
    integer :: parmei(2), parcri(3)
!
    real(kind=8) :: parmer(2), parcrr(2), para(2), valr(2)
    real(kind=8) :: tpsthe(6), tps1(4), deltat, deltam
    real(kind=8) :: theta, instap
!
    aster_logical :: matcst, coecst, lostat, levol, asme, asms, finpas, lnonl
    aster_logical :: reasvc, reasvt, reasmt, reasrg, reasms, force
!
    character(len=1) :: creas
    character(len=8) :: evolsc, mailla
    character(len=19) :: maprec, solveu, lischa, lisch2, sddisc, sdcrit
    character(len=24) :: result, modele, charge, carele
    character(len=24) :: fomult, numedd
    character(len=24) :: mediri, matass
    character(len=24) :: cndirp, cnchci, time
    character(len=24) :: infoch, mate
    character(len=24) :: vec2nd
    character(len=24) :: compor, sdieto
!
! ----------------------------------------------------------------------
!
    data result  /' '/
    data sdcrit  /'&&OP0025.CRIT.'/
    data mediri  /' '/
    data cndirp  /' '/
    data cnchci  /' '/
    data tpsthe  /6*0.d0/
    data sddisc  /'&&OP0025.SDDISC'/
    data sdieto  /'&&OP0186.SDIETO'/
!
! DEB ------------------------------------------------------------------
!====
! 1. PREALABLES
!====
!
    call jemarq()
!
!-----RECUPERATION DU NIVEAU D'IMPRESSION
!
    call infmaj()
    call infniv(ifm, niv)
!
!-----------------------------------------------------------------------
    asme = .true.
    asms = .false.
!
! 1.2. ==> NOM DES STRUCTURES
!
    solveu = '&&'//nompro//'.SOLVEUR   '
    lischa = '&&'//nompro//'_INFCHA    '
    maprec = '&&'//nompro//'_MAT_PRECON'
    vec2nd = '&&'//nompro//'_2ND_MEMBRE     '
    matass = '&&'//nompro//'_MATR_ASSEM     '
!
!====
! 2. LES DONNEES
!====
!
! 2.1. ==> LECTURE DES OPERANDES DE LA COMMANDE
!
    call nxlect(result, modele, mate, carele, matcst,&
                coecst, fomult, lischa, charge, infoch,&
                parmei, parmer, solveu, parcri, parcrr,&
                compor, evolsc)
    para(1) = parmer(1)
!
! 2.3. ==> LECTURE DE L'ETAT INITIAL ET DES DONNEES D'INCREMENTATION
!
    call ntinit(result, modele, mate, carele, lischa,&
                lisch2, solveu, para, numedd, lostat,&
                levol, lnonl, sddisc, sdieto, mailla,&
                sdcrit, time)       
    ASSERT(.not.lnonl)
!
! 2.5. ==> CALCUL DES MATRICES ELEMENTAIRES DES DIRICHLETS
!
    call medith(modele, charge, infoch, mediri)
!
! 2.6. ==> PILOTAGE DES REACTUALISATIONS DES ASSEMBLAGES
!     REASVT : INSTANTS, DIRICHLET, TERMES DU TRANSITOIRE
!     REASVC : CHARGEMENTS (SOURCES, FLUX, ... )
!     REASRG : MATRICE DE RIGIDITE
!     REASMS : MATRICE DE MASSE
!     REASMT : MATRICE TANGENTE POUR LE NON-LINEAIRE (SANS OBJET ICI)
!
    reasvt = .true.
    reasvc = .true.
    reasrg = .false.
    reasms = .false.
    reasmt = .false.
!
!
    if (lostat) then
        asms = .true.
        numins=0
    else
        numins=1
    endif
!
    deltat=-1.d150
!
! 2.6. ==> CREATION DES STRUCTURES
!
    call vtcreb(vec2nd, numedd, 'V', 'R', neq)
!
!====
! 3. BOUCLES SUR LES PAS DE TEMPS
!====
!
    call uttcpu('CPU.OP0025', 'INIT', ' ')
!
200 continue
!
! --- RECUPERATION DU PAS DE TEMPS ET DES PARAMETRES DE RESOLUTION
!
! --- CETTE BOUCLE IF SERT A MAINTENIR LE NUMERO D ORDRE
! --- A 1 DANS LE CAS D UN CALCUL STATIONNAIRE.
! --- IL FAUT EN EFFET PROCEDER A CE CALCUL AVANT DE PARCOURIR
! --- LA LISTE D INSTANT DE LA SD SDDISC
!
    if (lostat) then
        if (.not.levol) then
            instap=0.d0
            deltam=deltat
            deltat=-1.d150
            theta=1.d0
        else
            instap=diinst(sddisc, numins)
            deltam=deltat
            deltat=-1.d150
            theta=1.d0
        endif
    else
        instap = diinst(sddisc, numins)
        deltam=deltat
        deltat = instap-diinst(sddisc, numins-1)
        theta=parmer(1)
    endif
    para(2) = deltat
! --- MATRICE TANGENTE REACTUALISEE POUR UN NOUVEAU DT
!
    call uttcpu('CPU.OP0025', 'DEBUT', ' ')
    tpsthe(1) = instap
    tpsthe(2) = deltat
    tpsthe(3) = theta
!
    if ((.not.matcst.or..not.coecst) .or. asms .or. asme) then
        reasrg = .true.
        asms = .false.
    endif
    if ((.not.matcst) .or. deltam .ne. deltat) then
        reasms = .true.
        asme = .false.
    endif
!
! 3.2.2.2. ==> RESOLUTION
!
    call ntreso(modele, mate, carele, fomult, charge,&
                lischa, infoch, numedd, solveu, lostat,&
                time, tpsthe, reasvc, reasvt, reasmt,&
                reasrg, reasms, creas, vec2nd, matass,&
                maprec, cndirp, cnchci, mediri, compor)
!
    reasrg = .false.
    reasms = .false.
!
!
! ------- ARCHIVAGE
!
    if (lostat) then
        force = .true.
    else
        force = .false.
    endif
    call ntarch(numins, modele, mate, carele, lnonl,&
                para, sddisc, sdcrit, sdieto, lisch2,&
                force)
!
! ------- VERIFICATION SI INTERRUPTION DEMANDEE PAR SIGNAL USR1
!
    if (etausr() .eq. 1) then
        call sigusr()
    endif
!
! 3.2.3. ==> GESTION DU TEMPS CPU
!
    finpas = didern(sddisc, numins)
!
    call uttcpu('CPU.OP0025', 'FIN', ' ')
    call uttcpr('CPU.OP0025', 4, tps1)
    if (tps1(4) .gt. .95d0*tps1(1)-tps1(4)) then
        vali = numins
        valr(1) = tps1(4)
        valr(2) = tps1(1)
        call utmess('Z', 'ALGORITH16_68', si=vali, nr=2, valr=valr,&
                    num_except=28)
    else
        write (ifm,'(A,1X,I6,2(1X,A,1X,1PE11.3))') 'NUMERO D''ORDRE:',&
        numins,'INSTANT:',instap, 'DUREE MOYENNE:',tps1(4)
    endif
    if (lostat) then
        lostat=.false.
    endif
    numins = numins + 1
!
    if (finpas) goto 41
!
    goto 200
!
!
 41 continue
!
!
    call titre()
!
! --- DESTRUCTION DE TOUTES LES MATRICES CREEES
!
    call detmat()
!
! --- POST TRAITEMENT SPECIFIQUE X-FEM : CALCUL / STOCKAGE DE TEMP_ELGA
!
    call exixfe(modele, iret)
    if (iret .ne. 0) then
        call xthpos(result, modele)
    endif
!
    call jedema()
!
end subroutine
