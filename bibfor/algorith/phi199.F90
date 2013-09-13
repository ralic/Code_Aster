subroutine phi199(model, mate, ma, nu, num,&
                  nbmode, solvez, indice, tabad)
    implicit none
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/calflu.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupc.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/majou.h"
#include "asterfort/prstoc.h"
#include "asterfort/pteddl.h"
#include "asterfort/reliem.h"
#include "asterfort/resoud.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsorac.h"
#include "asterfort/rsvpar.h"
#include "asterfort/tabcor.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mess.h"
#include "asterfort/vtcreb.h"
#include "asterfort/wkvect.h"
    integer :: nbmode, indice, tabad(*)
    character(len=8) :: ma
    character(len=14) :: nu, num
    character(len=*) :: mate, solvez
!---------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!---------------------------------------------------------------------
!
!
! CALCULS DES CONDITIONS AUX LIMITES POUR LA DETERMINATION
! DES POTENTIELS FLUCTUANTS POUR LA FORCE AJOUTEE,
! IN : K* : MODEL : TYPE DE MODELISATION FLUIDE
! IN : K* : MATE : MATERIAU FLUIDE
! IN : K* : PHIBAR : NOM DU POTENTIEL PERMANENT
! IN : K* : MA : NOM DE LA MATRICE DE RAIDEUR FLUIDE
! IN : K* : NU : NUMEROTATION DES DDLS ASSOCIES AU FLUIDE
! IN : K* : NUM : NUMEROTATION DES DDLS ASSOCIES A L'INTERFACE
!           POTENTIELS FLUCTUANTS : 1 : MASSE AJOUTEE
!                                 : 2 : AMORTISSEMENT ET RAIDEUR
! IN : K* : SOLVEZ : METHODE DE RESOLUTION 'MULT_FRONT','LDLT' OU 'GCPC'
!---------------------------------------------------------------------
    integer :: ibid, nbvale, nbrefe, nbdesc, iret, nbno, idno, id, ier
    integer :: ilires, jref, neq, nbd, nbdir, i, jvec, jddl, in, nbsta
    integer :: iphi1, n3, n1, icor(2), n2, ndble, iordr, nbtrou, idmst
    real(kind=8) :: rbid, xnorm, xd, depl(6), epsi
    complex(kind=8) :: c16b, cbid
    character(len=2) :: model
    character(len=8) :: k8bid, modmec, mailla, maflui, tabcmp(6), crit
    character(len=8) :: moflui, moint, typmcl(2), modsta
    character(len=14) :: nume
    character(len=16) :: acces, motcle(2)
    character(len=19) :: vecso1, vesto1, maprec, solveu, chsol, chamno
    character(len=24) :: nomcha, nocham, criter
    character(len=24) :: valk(3)
!
    data maprec   /'&&OP0199.MAPREC'/
    data chsol    /'&&OP0199.SOLUTION'/
    data   tabcmp / 'DX' , 'DY' , 'DZ' , 'DRX' , 'DRY' , 'DRZ' /
    data ndble /0/
! -----------------------------------------------------------------
!
    call jemarq()
    epsi = r8prem( )
    ier = 0
    solveu = solvez
    criter = '&&RESGRA_GCPC'
    indice=0
!
    call getvid(' ', 'MODELE_FLUIDE', scal=moflui, nbret=n1)
    call getvid(' ', 'MODELE_INTERFACE', scal=moint, nbret=n2)
    call getvid(' ', 'MODE_MECA', scal=modmec, nbret=n3)
!
! --- TEST POUR DETERMINER SI FLUIDE ET STRUCTURE S APPUIENT SUR
!     DES MAILLAGES COMMUNS
!
    if (n3 .gt. 0) then
        call rsorac(modmec, 'LONUTI', ibid, rbid, k8bid,&
                    cbid, rbid, 'ABSOLU', nbmode, 1,&
                    ibid)
        call rsexch('F', modmec, 'DEPL', 1, nomcha,&
                    iret)
        call dismoi('F', 'NOM_MAILLA', nomcha, 'CHAM_NO', ibid,&
                    mailla, ier)
        call dismoi('F', 'NOM_MAILLA', moint, 'MODELE', ibid,&
                    maflui, ier)
        if (maflui .ne. mailla) then
            call tabcor(model, mate, mailla, maflui, moint,&
                        num, ndble, icor)
            call majou(model, modmec, solveu, num, nu,&
                       ma, mate, moint, ndble, icor,&
                       tabad)
            indice=1
        endif
    endif
!
!=====================================================================
!---------------- ALTERNATIVE MODE_MECA OU---------
!-----------------------------MODELE-GENE--------------------
!=====================================================================
! DANS LE CAS OU ON N A PAS CALCUL DE MASSE AJOUTEE SUR UN
! MAILLAGE SQUELETTE
!
    if ((n3.gt.0) .and. (indice.ne.1)) then
!
!
!----- -RECUPERATION DU NB DE MODES DU CONCEPT MODE_MECA
!
        call rsorac(modmec, 'LONUTI', ibid, rbid, k8bid,&
                    cbid, rbid, 'ABSOLU', nbmode, 1,&
                    ibid)
!
        call wkvect('&&OP0199.PHI1', 'V V K24', 1, iphi1)
!
!======================================================================
! BOUCLE SUR LE NOMBRE DE MODES: CALCUL DU FLUX FLUIDE MODAL
!======================================================================
        ilires = 0
        nomcha = '&&PHI199.CHAMREF'
        call rsexch('F', modmec, 'DEPL', 1, nocham,&
                    iret)
        nocham = nocham(1:19)//'.REFE'
        call jeveuo(nocham, 'L', jref)
        nume = zk24(jref+1)(1:14)
        call vtcreb(nomcha, nume, 'V', 'R', neq)
!
! --- QUELLE EST LA DIRECTION ?
!
        call getvr8(' ', 'DIRECTION', nbval=0, nbret=nbd)
        nbdir = -nbd
        call getvr8(' ', 'DIRECTION', nbval=nbdir, vect=depl, nbret=nbd)
!
!     --- ON NORMALISE LE VECTEUR ---
        xnorm = 0.d0
        do 10 i = 1, nbdir
            xnorm = xnorm + depl(i) * depl(i)
10      continue
        xnorm = sqrt(xnorm)
        if (xnorm .lt. 0.d0) then
            call u2mess('F', 'ALGORITH9_81')
        endif
        do 12 i = 1, nbdir
            depl(i) = depl(i) / xnorm
12      continue
!
        call jeveuo(nomcha(1:19)//'.VALE', 'E', jvec)
        call wkvect('&&PHI199.DDL', 'V V I', neq*nbdir, jddl)
        call pteddl('NUME_DDL', nume, nbdir, tabcmp, neq,&
                    zi(jddl))
!
        do 21 in = 0, neq-1
            zr(jvec+in) = 0.d0
21      continue
!
!     --- ON RECUPERE LES MODES STATIQUES ---
!
        call getvid(' ', 'MODE_STAT', scal=modsta, nbret=nbsta)
        if (nbsta .eq. 0) goto 41
!
!     --- ON RECUPERE LES POINTS D'ANCRAGE ---
!
        motcle(1) = 'NOEUD'
        motcle(2) = 'GROUP_NO'
        typmcl(1) = 'NOEUD'
        typmcl(2) = 'GROUP_NO'
        call reliem(' ', mailla, 'NO_NOEUD', ' ', 1,&
                    2, motcle, typmcl, '&&PHI199.NOEUD', nbno)
        call jeveuo('&&PHI199.NOEUD', 'L', idno)
!
!     --- ON BOUCLE SUR LES NOEUDS ---
!
        do 25 id = 1, nbdir
            xd = depl(id)
            if (abs(xd) .gt. epsi) then
                do 26 in = 1, nbno
                    acces(1:8 ) = zk8(idno+in-1)
                    acces(9:16) = tabcmp(id)
!
!              --- ON RECUPERE LE MODE STATIQUE ASSOCIE AU NOEUD ---
                    call rsorac(modsta, 'NOEUD_CMP', ibid, rbid, acces,&
                                c16b, epsi, crit, iordr, 1,&
                                nbtrou)
                    if (nbtrou .ne. 1) then
                        ier = ier + 1
                        valk (1) = acces(1:8)
                        valk (2) = acces(9:16)
                        call u2mesg('E', 'ALGELINE4_61', 2, valk, 0,&
                                    0, 0, 0.d0)
                        goto 26
                    endif
                    call rsvpar(modsta, iordr, 'TYPE_DEFO', ibid, rbid,&
                                'DEPL_IMPO', iret)
                    if (iret .ne. 100) then
                        ier = ier + 1
                        valk (1) = 'DDL_IMPO'
                        valk (2) = acces(1:8)
                        valk (3) = acces(9:16)
                        call u2mesg('E', 'ALGELINE4_62', 3, valk, 0,&
                                    0, 0, 0.d0)
                        goto 26
                    endif
                    call rsexch('F', modsta, 'DEPL', iordr, chamno,&
                                iret)
                    call jeveuo(chamno//'.VALE', 'L', idmst)
!
                    do 27 i = 0, neq-1
                        zr(jvec+i) = zr(jvec+i) - zi(jddl+(id-1)*neq+ i)*xd*zr(idmst+i)
27                  continue
                    call jelibe(chamno//'.VALE')
26              continue
            endif
25      continue
        if (ier .ne. 0) then
            call u2mess('F', 'ALGORITH5_24')
        endif
!
        goto 42
!
41      continue
        do 23 i = 1, nbdir
            do 24 in = 0, neq-1
                zr(jvec+in) = zr(jvec+in) - zi(jddl+(i-1)*neq+in)* depl(i)
24          continue
23      continue
42      continue
!
        nomcha = nomcha(1:19)
        vecso1 = '&&OP0199.VECSOL1'
!
        call calflu(nomcha, moflui, mate, nu, vecso1,&
                    nbdesc, nbrefe, nbvale, 'R')
!
        ilires = ilires + 1
!
!------------- RESOLUTION  DU LAPLACIEN EN 2D-----------------------
!
        call resoud(ma, maprec, solveu, ' ', 0,&
                    vecso1, chsol, 'V', rbid, cbid,&
                    criter, .true., 0, iret)
        call jedupc('V', chsol(1:19), 1, 'V', vecso1(1:19),&
                    .false.)
        call detrsd('CHAMP_GD', chsol)
!
!------------ CREATION DU VECTEUR PRESSION MODAL-------------------
!
!- FORMATION DU TABLEAU CONTENANT LA PRESSION POUR CHAQUE MODE-----
!
!------------------------------------------------------------------
        vesto1 = '&&OP0199.VEST1'
        call prstoc(vecso1, vesto1, ilires, ilires, iphi1,&
                    nbvale, nbrefe, nbdesc)
!
    endif
!
! --- MENAGE
    call detrsd('CHAM_NO', '&&PHI199.CHAMREF')
    call jedetr('&&PHI199.DDL')
    call jedetr('&&PHI199.NOEUD')
!
    call jeexin(criter(1:19)//'.CRTI', iret)
    if (iret .ne. 0) then
        call jedetr(criter(1:19)//'.CRTI')
        call jedetr(criter(1:19)//'.CRTR')
        call jedetr(criter(1:19)//'.CRDE')
    endif
!----------------------------------------------------------------
    call jedema()
end subroutine
