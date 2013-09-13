subroutine mdnewm(nbpas, dt, nbmode, pulsat, pulsa2,&
                  masgen, riggen, rgygen, lamor, amogen,&
                  gyogen, foncv, fonca, typbas, basemo,&
                  tinit, iparch, depsto, vitsto, accsto,&
                  iorsto, temsto, nomres, nbexci, idescf,&
                  nomfon, coefm, liad, inumor, passto)
! aslint: disable=W1504
    implicit none
#include "jeveux.h"
#include "asterc/etausr.h"
#include "asterfort/fointe.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedisp.h"
#include "asterfort/jemarq.h"
#include "asterfort/mdacce.h"
#include "asterfort/mdarch.h"
#include "asterfort/mdfext.h"
#include "asterfort/mdinit.h"
#include "asterfort/mdsize.h"
#include "asterfort/pmavec.h"
#include "asterfort/resu74.h"
#include "asterfort/rrlds.h"
#include "asterfort/sigusr.h"
#include "asterfort/trlds.h"
#include "asterfort/u2mess.h"
#include "asterfort/utexcm.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
#include "asterfort/wkvect.h"
#include "blas/dcopy.h"
    integer :: iorsto(*), iparch(*), idescf(*)
    real(kind=8) :: pulsat(*), pulsa2(*), masgen(*), riggen(*), amogen(*)
    real(kind=8) :: gyogen(*), depsto(*), vitsto(*), accsto(*), temsto(*)
    real(kind=8) :: rgygen(*)
    character(len=8) :: basemo, nomres, nomfon(*), foncv, fonca
    character(len=16) :: typbas
    logical :: lamor
    integer :: descmm, descmr, descma, liad(*), inumor(*)
    real(kind=8) :: r8b, coefm(*), passto(*)
!-----------------------------------------------------------------------
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
!
!
!     ALGORITHME DE NEWMARK
!     ------------------------------------------------------------------
! IN  : NBPAS  : NOMBRE DE PAS
! IN  : DT     : PAS DE TEMPS
! IN  : NBMODE : NOMBRE DE MODES
! IN  : PULSAT : PULSATIONS MODALES
! IN  : PULSA2 : PULSATIONS MODALES AU CARREES
! IN  : MASGEN : MASSES GENERALISEES ( TYPBAS = 'MODE_MECA' )
!                MATRICE DE MASSE GENERALISEE ( TYPBAS = 'BASE_MODA' )
! IN  : RIGGEN : RAIDEURS GENERALISES ( TYPBAS = 'MODE_MECA' )
!                MATRICE DE RAIDEUR GENERALISE ( TYPBAS = 'BASE_MODA' )
! IN  : LAMOR  : AMORTISSEMENT SOUS FORME D'UNE LISTE DE REELS
! IN  : AMOGEN : AMORTISSEMENTS REDUITS ( LAMOR = .TRUE. )
!                MATRICE D'AMORTISSEMENT ( LAMOR = .FALSE. )
! IN  : TYPBAS : TYPE DE LA BASE 'MODE_MECA' OU 'BASE_MODA'
! IN  : NBEXCI : NBRE D'EXCITATIONS (SOUS LE MC EXCIT ET EXCIT_RESU)
! IN  : IDESCF : TYPE D'EXCITATION (VECT_ASSE/NUME_ORDRE,FONC_MULT/
!                COEF_MULT)
! IN  : NOMFON : NOM DES FONC_MULT (QUAND IL Y EN A)
! IN  : COEFM  : VALEUR DU COEF_MULT
! IN  : LIAD   : VALEUR DU VECT_ASSE
! IN  : NUMOR  : NUME_ORDRE DU MODE EXCITE
! ----------------------------------------------------------------------
!
    real(kind=8) :: tps1(4), valr(3), beta, gamma, res, tol, acce
    real(kind=8) :: vrot, vrotin, arot, arotin
    integer :: vali(2), n1, ifm, niv
    character(len=8) :: tran, vvar
    character(len=4) :: k4bid
!     ------------------------------------------------------------------
!
! 1.1. ==> RECUPERATION DU NIVEAU D'IMPRESSION
!
!-----------------------------------------------------------------------
    integer :: i, ia, iarchi, ib, ibid, ier
    integer :: if, ife, im, im1, ind, ipas, ipm(1)
    integer :: iret, isto1, jacce, jdepl, jfext, jm, jmass
    integer :: jtra1, jtra2, jtra3, jtra4, jtra5, jtra6, jvite
    integer :: n100, nbbloc, nbexci, nbmod1, nbmode, nbpas, nbpasb
    integer :: nbpasf, nbpp, ndim, ndt, jamgy, jrigy
    real(kind=8) :: a0, a1, a2, a3, a4, a5, a6
    real(kind=8) :: a7, deux, dt, dt2, tarchi, temps, tinit
    real(kind=8) :: x1, x2, x3, zero
    complex(kind=8) :: cbid
!-----------------------------------------------------------------------
    call infniv(ifm, niv)
!
    call jemarq()
!
    zero = 0.d0
    deux = 2.d0
    vvar = 'NON'
    dt2 = dt * dt
!
    call getvr8('SCHEMA_TEMPS', 'BETA', iocc=1, scal=beta, nbret=n1)
    call getvr8('SCHEMA_TEMPS', 'GAMMA', iocc=1, scal=gamma, nbret=n1)
    res = 0.25d0* (0.5d0+gamma)* (0.5d0*gamma)
    tol = 1.d-8
    if (gamma .lt. (0.5d0-tol) .or. beta .lt. (res-tol)) then
        write (ifm,*) ' >>> NEWMARK <<<'//&
     &      'CAS CONDITIONNELLEMENT STABLE.'
    endif
    if (beta .eq. 0) then
        call u2mess('F', 'ALGORITH9_2')
    endif
!
    a0 = 1.d0/beta/dt2
    a1 = gamma/beta/dt
    a2 = 1.d0/beta/dt
    a3=1.d0/(2*beta)-1
    a4=gamma/beta-1
    a5=dt/2*(gamma/beta-2)
    a6=dt*(1-gamma)
    a7=gamma*dt
!
    isto1 = 0
    r8b = zero
    nbmod1 = nbmode - 1
!
!     --- VECTEURS DE TRAVAIL ---
    call wkvect('&&MDNEWM.DEPL', 'V V R8', nbmode, jdepl)
    call wkvect('&&MDNEWM.VITE', 'V V R8', nbmode, jvite)
    call wkvect('&&MDNEWM.ACCE', 'V V R8', nbmode, jacce)
    call wkvect('&&MDNEWM.TRA1', 'V V R8', nbmode, jtra1)
    call wkvect('&&MDNEWM.TRA2', 'V V R8', nbmode, jtra2)
    call wkvect('&&MDNEWM.KTILDA', 'V V R8', nbmode*nbmode, jtra3)
    call wkvect('&&MDNEWM.FTILD1', 'V V R8', nbmode*nbmode, jtra4)
    call wkvect('&&MDNEWM.FTILD2', 'V V R8', nbmode*nbmode, jtra5)
    call wkvect('&&MDNEWM.FTILD3', 'V V R8', nbmode*nbmode, jtra6)
    call wkvect('&&MDNEWM.AMOGYR', 'V V R8', nbmode*nbmode, jamgy)
    call wkvect('&&MDNEWM.RIGGYR', 'V V R8', nbmode*nbmode, jrigy)
!
!     --- A-T-ON ASSEZ DE PLACE POUR CREER LE VECTEUR "FEXT" ? ---
    call jedisp(1, ipm)
    ndim = nbmode * nbpas
    if (ndim .le. ipm(1)) then
!        --- ON ALLOUE LE VECTEUR ---
        nbbloc = 1
        nbpasb = nbpas
        nbpasf = nbpas
        call wkvect('&&MDNEWM.FEXT', 'V V R8', ndim, jfext)
    else
!        --- DECOUPAGE EN BLOC ---
        nbbloc = ndim / ipm(1)
        nbpasb = ipm(1) / nbmode
        nbpasf = nbpas - ( nbbloc * nbpasb )
        nbbloc = nbbloc + 1
        ndim = nbmode * nbpasb
        call wkvect('&&MDNEWM.FEXT', 'V V R8', ndim, jfext)
        write(6,*)'--->> MDNEWM: DECOUPAGE, NBBLOC=',nbbloc
        write(6,*)'--->>                    NBPASB=',nbpasb
        write(6,*)'--->>                    NBPASF=',nbpasf
    endif
!
    if (typbas .eq. 'MODE_MECA' .or. typbas .eq. 'MODE_GENE') then
        if (lamor) then
            do 100 im = 1, nbmode
                amogen(im) = deux * amogen(im) * pulsat(im)
100          continue
        else
            do 110 im = 1, nbmode
                do 112 jm = 1, nbmode
                    ind = jm + nbmode*(im-1)
                    zr(jtra3+ind-1) = a1 * amogen(ind)
112              continue
                ind = im + nbmode*(im-1)
                zr(jtra3+ind-1) = zr(jtra3+ind-1) + a0*masgen(im) + riggen(im)
110          continue
!           --- FACTORISATION DE LA MATRICE KTILDA ---
            call trlds(zr(jtra3), nbmode, nbmode, iret)
            if (iret .ne. 0) call u2mess('F', 'ALGORITH5_61')
        endif
    else
        if (lamor) then
            do 120 im = 1, nbmode
                amogen(im) = deux * amogen(im) * pulsat(im)
                do 122 jm = 1, nbmode
                    ind = jm + nbmode*(im-1)
                    zr(jtra3+ind-1) = a0*masgen(ind) + riggen(ind)
                    zr(jtra4+ind-1) = a2*masgen(ind)
                    zr(jtra5+ind-1) = a0*masgen(ind)
                    zr(jtra6+ind-1) = a3*masgen(ind)
122              continue
                ind = im + nbmode*(im-1)
                zr(jtra3+ind-1) = zr(jtra3+ind-1) + a1*amogen(im)* masgen(ind)
                zr(jtra4+ind-1) = zr(jtra4+ind-1) + a4*amogen(im)* masgen(ind)
                zr(jtra5+ind-1) = zr(jtra5+ind-1) + a1*amogen(im)* masgen(ind)
                zr(jtra6+ind-1) = zr(jtra6+ind-1) + a5*amogen(im)* masgen(ind)
120          continue
        else
            call getvtx(' ', 'VITESSE_VARIABLE', nbval=0, nbret=n1)
            if (n1 .ne. 0) then
                call getvtx(' ', 'VITESSE_VARIABLE', scal=vvar, nbret=n1)
            endif
            vrotin = 0.d0
            arotin = 0.d0
            if (vvar .eq. 'OUI') then
                call fointe('F ', foncv, 1, 'INST', tinit,&
                            vrotin, ier)
                call fointe('F ', fonca, 1, 'INST', tinit,&
                            arotin, ier)
                do 113 im = 1, nbmode
                    do 114 jm = 1, nbmode
                        ind = jm + nbmode*(im-1)
                        zr(jamgy+ind-1) = amogen(ind)+vrotin*gyogen( ind)
                        zr(jrigy+ind-1) = riggen(ind)+arotin*rgygen( ind)
114                  continue
113              continue
            else
                do 117 im = 1, nbmode
                    do 118 jm = 1, nbmode
                        ind = jm + nbmode*(im-1)
                        zr(jamgy+ind-1) = amogen(ind)
                        zr(jrigy+ind-1) = riggen(ind)
118                  continue
117              continue
            endif
            do 130 im = 1, nbmode
                do 132 jm = 1, nbmode
                    ind = jm + nbmode*(im-1)
                    zr(jtra3+ind-1) = a0*masgen(ind) + zr(jrigy+ind-1) + a1*zr(jamgy+ind-1)
                    zr(jtra4+ind-1) = a2*masgen(ind) + a4*zr(jamgy+ ind-1)
                    zr(jtra5+ind-1) = a0*masgen(ind) + a1*zr(jamgy+ ind-1)
                    zr(jtra6+ind-1) = a3*masgen(ind) + a5*zr(jamgy+ ind-1)
132              continue
130          continue
        endif
!        --- FACTORISATION DE LA MATRICE MASSE ---
        call wkvect('&&MDNEWM.MASS', 'V V R8', nbmode*nbmode, jmass)
        call dcopy(nbmode*nbmode, masgen, 1, zr(jmass), 1)
        call trlds(zr(jmass), nbmode, nbmode, iret)
!         CALL TRLDS(MASGEN,NBMODE,NBMODE,IRET)
        if (iret .ne. 0) call u2mess('F', 'ALGORITH5_22')
!        --- FACTORISATION DE LA MATRICE KTILDA ---
        call trlds(zr(jtra3), nbmode, nbmode, iret)
        if (iret .ne. 0) call u2mess('F', 'ALGORITH5_61')
    endif
!
!     --- CONDITIONS INITIALES ---
    call mdinit(basemo, nbmode, 0, zr(jdepl), zr(jvite),&
                r8b, iret, tinit)
    if (iret .ne. 0) goto 9999
!
!     --- FORCES EXTERIEURES ---
    if (nbexci .ne. 0) then
        call mdfext(tinit, dt, nbmode, nbexci, idescf,&
                    nomfon, coefm, liad, inumor, 1,&
                    zr(jfext))
    endif
!
!     --- ACCELERATIONS GENERALISEES INITIALES ---
    call mdacce(typbas, nbmode, pulsa2, masgen, descmm,&
                riggen, descmr, zr(jfext), lamor, amogen,&
                descma, zr(jtra1), zr(jdepl), zr(jvite), zr(jacce))
!
!     --- ARCHIVAGE DONNEES INITIALES ---
    tarchi = tinit
!
    call mdarch(isto1, 0, tinit, dt, nbmode,&
                'TRAN', ibid, k4bid, zr(jdepl), zr(jvite),&
                zr(jacce), depsto, vitsto, accsto, cbid,&
                cbid, cbid, cbid, cbid, cbid,&
                passto, iorsto, temsto)
!
    temps = tinit + dt
    call uttcpu('CPU.MDNEWM', 'INIT', ' ')
    n100 = nbpas/100 + 1
    ipas = 0
!
!     --- BOUCLE TEMPORELLE ---
    do 10 ib = 1, nbbloc
        ia = ( ib - 1 ) * nbpasb
        if (ib .eq. nbbloc) then
            nbpp = nbpasf
        else
            nbpp = nbpasb
        endif
        do 20 if = 0, ndim-1
            zr(jfext+if) = zero
20      continue
!
!        --- FORCES EXTERIEURES ---
        if (nbexci .ne. 0) then
            call mdfext(temps, dt, nbmode, nbexci, idescf,&
                        nomfon, coefm, liad, inumor, nbpp,&
                        zr(jfext))
        endif
!
        do 200 i = 1, nbpp
!
            if (mod(ipas,n100) .eq. 0) call uttcpu('CPU.MDNEWM', 'DEBUT', ' ')
!
            ife = ( i - 1 ) * nbmode
            if (typbas .eq. 'MODE_MECA' .or. typbas .eq. 'MODE_GENE') then
                if (lamor) then
                    do 210 im = 0, nbmod1
                        im1 = im + 1
                        zr(jtra1+im) = zr(jdepl+im)
                        x1 = ( a2 + a4*amogen(im1) ) * masgen(im1)
                        x2 = ( a0 + a1*amogen(im1) ) * masgen(im1)
                        x3 = x2 + riggen(im1)
                        zr(jdepl+im) = (&
                                       zr(jfext+ife+im) + x1*zr( jvite+im) + a3*masgen(im1)*zr(ja&
                                       &cce+im) + a5*amogen(im1)*zr(jacce+im) + x2*zr(jdepl+ im)&
                                       ) / x3
210                  continue
                else
                    do 212 im = 0, nbmod1
                        zr(jtra1+im) = zr(jdepl+im)
                        zr(jtra2+im) = a4*zr(jvite+im) + a1*zr(jdepl+ im) +a5* zr(jacce+im)
212                  continue
                    call pmavec('ZERO', nbmode, amogen, zr(jtra2), zr( jdepl))
                    do 214 im = 0, nbmod1
                        im1 = im + 1
                        x1 = a3*zr(jacce+im) + a2*zr(jvite+im) + a0*zr(jtra1+im)
                        zr(jdepl+im) = zr(jdepl+im) + zr(jfext+ife+im) + x1*masgen(im1)
214                  continue
                    call rrlds(zr(jtra3), nbmode, nbmode, zr(jdepl), 1)
                endif
            else
                vrot = 0.d0
                arot = 0.d0
                if (vvar .eq. 'OUI') then
                    call fointe('F ', foncv, 1, 'INST', temps,&
                                vrot, ier)
                    call fointe('F ', fonca, 1, 'INST', temps,&
                                arot, ier)
                    do 115 im = 1, nbmode
                        do 116 jm = 1, nbmode
                            ind = jm + nbmode*(im-1)
                            zr(jamgy+ind-1) = amogen(ind) + vrot* gyogen(ind)
                            zr(jrigy+ind-1) = riggen(ind) + arot* rgygen(ind)
116                      continue
115                  continue
                    do 135 im = 1, nbmode
                        do 136 jm = 1, nbmode
                            ind = jm + nbmode*(im-1)
                            zr(jtra3+ind-1) = a0*masgen(ind) + zr( jrigy+ind-1) + a1*zr(jamgy+ind&
                                              &-1)
                            zr(jtra4+ind-1) = a2*masgen(ind)+a4*zr( jamgy+ind-1)
                            zr(jtra5+ind-1) = a0*masgen(ind)+a1*zr( jamgy+ind-1)
                            zr(jtra6+ind-1) = a3*masgen(ind)+a5*zr( jamgy+ind-1)
136                      continue
135                  continue
                    call trlds(zr(jtra3), nbmode, nbmode, iret)
                endif
                do 216 im = 0, nbmod1
                    zr(jtra1+im) = zr(jdepl+im)
                    zr(jdepl+im) = zr(jfext+ife+im)
216              continue
                call pmavec('CUMUL', nbmode, zr(jtra6), zr(jacce), zr( jdepl))
                call pmavec('CUMUL', nbmode, zr(jtra4), zr(jvite), zr( jdepl))
                call pmavec('CUMUL', nbmode, zr(jtra5), zr(jtra1), zr( jdepl))
                call rrlds(zr(jtra3), nbmode, nbmode, zr(jdepl), 1)
            endif
            do 218 im = 0, nbmod1
                acce=zr(jacce+im)
                zr(jacce+im) = -a3*acce + a0*( zr(jdepl+im) - zr( jtra1+im) - dt*zr(jvite+im))
!
                zr(jvite+im) = zr(jvite+im) + a6*acce + a7*zr(jacce+ im)
!
218          continue
!
!           --- ARCHIVAGE ---
            iarchi = ia + i
            if (iparch(iarchi) .eq. 1) then
                isto1 = isto1 + 1
                temps = tinit + iarchi*dt
                tarchi = temps
!
                call mdarch(isto1, iarchi, temps, dt, nbmode,&
                            'TRAN', ibid, k4bid, zr(jdepl), zr(jvite),&
                            zr(jacce), depsto, vitsto, accsto, cbid,&
                            cbid, cbid, cbid, cbid, cbid,&
                            passto, iorsto, temsto)
!
            endif
!
!       --- VERIFICATION SI INTERRUPTION DEMANDEE PAR SIGNAL USR1 ---
!
            if (etausr() .eq. 1) then
                call sigusr()
            endif
!
!       --- TEST SI LE TEMPS RESTANT EST SUFFISANT POUR CONTINUER ---
!
            if (mod(ipas,n100) .eq. 0) then
                call uttcpu('CPU.MDNEWM', 'FIN', ' ')
                call uttcpr('CPU.MDNEWM', 4, tps1)
!
                if (max(5.d0,n100*tps1(4)) .gt. 0.90d0*tps1(1)) then
                    call mdsize(nomres, isto1, nbmode, 0, 0,&
                                0)
                    if (nomres .eq. '&&OP0074') then
!             --- CAS D'UNE POURSUITE ---
                        call getvid('ETAT_INIT', 'RESULTAT', iocc=1, scal=tran, nbret=ndt)
                        if (ndt .ne. 0) call resu74(tran, nomres)
                    endif
                    vali (1) = ia+i
                    vali (2) = isto1
                    valr (1) = tarchi
                    valr (2) = tps1(4)
                    valr (3) = tps1(1)
                    call utexcm(28, 'ALGORITH16_77', 0, ' ', 2,&
                                vali, 3, valr)
                    goto 9999
                endif
!
            endif
            ipas = ipas + 1
!
200      continue
!
        temps = tinit + ( ia + nbpasb + 1 )*dt
!
10  end do
!
9999  continue
    call jedetr('&&MDNEWM.DEPL')
    call jedetr('&&MDNEWM.VITE')
    call jedetr('&&MDNEWM.ACCE')
    call jedetr('&&MDNEWM.TRA1')
    call jedetr('&&MDNEWM.TRA2')
    call jedetr('&&MDNEWM.KTILDA')
    call jedetr('&&MDNEWM.FTILD1')
    call jedetr('&&MDNEWM.FTILD2')
    call jedetr('&&MDNEWM.FTILD3')
    call jedetr('&&MDNEWM.AMOGYR')
    call jedetr('&&MDNEWM.RIGGYR')
    call jedetr('&&MDNEWM.FEXT')
    if (iret .ne. 0) call u2mess('F', 'ALGORITH5_24')
!
    call jedema()
end subroutine
