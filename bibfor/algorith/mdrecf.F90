subroutine mdrecf(nexci, nexcir, idescf, nomfon, coefm,&
                  iadvec, inumor, fondep, fonvit, fonacc,&
                  neq, typbas, basemo, nbmode, riggen,&
                  nommot, nomres)
    implicit none
#include "jeveux.h"
#include "asterc/getvid.h"
#include "asterc/getvis.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jeveut.h"
#include "asterfort/rsexch.h"
#include "asterfort/trmult.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
#include "asterfort/zerlag.h"
    integer :: nexci, nexcir, neq, nbmode
    integer :: idescf(*), inumor(*), iadvec(*)
    real(kind=8) :: coefm(*), riggen(nbmode)
    character(len=8) :: nomfon(2*nexci)
    character(len=8) :: fondep(2*nexci), fonvit(2*nexci)
    character(len=8) :: fonacc(2*nexci)
    character(len=8) :: basemo, nommot, nomres
    character(len=16) :: typbas
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
!
!     CALCULE LES FORCES EXTERIEURES A CHAQUE PAS DE TEMPS
!     ------------------------------------------------------------------
! IN  : NEXCI  : NOMBRE D'EXCITATIONS MODALES
! IN  : NEXCIR : NOMBRE D'EXC MODALES DONNEES SOUS FORME DE TRAN_GENE
! IN  : IDESCF  :
! IN  : NOMFON  : NOM DE LA FONCTION EXCITATION
! IN  : COEFM   :
! IN  : IADVEC  :
! IN  : INUMOR  :
! OUT : FONDEP  : NOM DE LA FONCTION DEPLACEMENT
! OUT : FONVIT  : NOM DE LA FONCTION VITESSE
! OUT : FONACC  : NOM DE LA FONCTION ACCELERATION
! OUT : PSDEL  : VECTEUR DES PSI*DELTA OU CORRECTIONS MODALES
! IN  : NEQ    : NOMBRE D'EQUATIONS
! IN  : BASEMO : NOM DE LA BASE MODALE DE PROJECTION
! OUT : NOMMOT :OUI SI MULTI APPUIS OU CORRECTION MODALE
! ----------------------------------------------------------------------
!
!
!
!
    integer :: i, ier, ninst, jinst, jprol, nexcit
    real(kind=8) :: alpha
    real(kind=8) :: coef
    character(len=2) :: ires
    character(len=3) :: imo
    character(len=8) :: modsta, modcor, k8bid
    character(len=8) :: matass, mailla, monmot(2)
    character(len=14) :: numddl
    character(len=19) :: veasge, fonct, facce
    character(len=19) :: chamno, chamn2, nofk19, resu
    character(len=24) :: deeq, typeba
    integer :: iarg, jpsdel, npsdel, iipsdl
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ib, ibid, iddeeq, ieq, ii, iinst, imod
    integer :: imodco, inum, iret, jdepl, jdrif, jmod, jordr
    integer :: jvale, l1, lprol, m1, n1, n2, n3
    integer :: n4, n5, na, nbv, nf, nm
    complex(kind=8) :: cbid
!-----------------------------------------------------------------------
    call jemarq()
    ier = 0
! ---    CALCUL TRANSITOIRE CLASSIQUE
    call jeveuo(basemo//'           .REFD', 'L', jdrif)
    typeba=zk24(jdrif+6)
!
!
    if (typbas(1:9) .eq. 'MODE_MECA' .and. typeba(1:1) .eq. ' ') then
        matass = zk24(jdrif)(1:8)
        call dismoi('F', 'NOM_MAILLA', matass, 'MATR_ASSE', ib,&
                    mailla, ier)
        call dismoi('F', 'NOM_NUME_DDL', matass, 'MATR_ASSE', ibid,&
                    numddl, ier)
        deeq = numddl//'.NUME.DEEQ'
        call jeveuo(deeq, 'L', iddeeq)
    else if (typeba(1:1).ne.' ') then
        numddl = zk24(jdrif+3)(1:14)
        call dismoi('F', 'NOM_MAILLA', numddl, 'NUME_DDL', ib,&
                    mailla, ier)
        deeq = numddl//'.NUME.DEEQ'
        call jeveuo(deeq, 'L', iddeeq)
    endif
    nommot = 'NON'
!
    nexcit = nexci + nexcir*nbmode
    npsdel = nexcit*neq
!
! --- EXCITATIONS SOUS LE MOT-CLE EXCIT
!
    do 10 i = 1, nexci
!
        call getvis('EXCIT', 'NUME_ORDRE', i, iarg, 1,&
                    inum, nf)
        call getvid('EXCIT', 'VECT_ASSE_GENE', i, iarg, 1,&
                    veasge, l1)
        call getvid('EXCIT', 'FONC_MULT', i, iarg, 1,&
                    fonct, n1)
        call getvr8('EXCIT', 'COEF_MULT', i, iarg, 1,&
                    alpha, m1)
        call getvid('EXCIT', 'ACCE', i, iarg, 1,&
                    facce, na)
        call getvtx('EXCIT', 'MULT_APPUI', i, iarg, 1,&
                    monmot(1), n2)
        call getvtx('EXCIT', 'CORR_STAT', i, iarg, 1,&
                    monmot(2), n3)
!
        if (n1 .ne. 0) then
!         CAS D'UNE FONC_MULT
            nomfon(i) = fonct(1:8)
            call jeveuo(fonct//'.PROL', 'L', lprol)
            nomfon(i+nexcit) = zk24(lprol)(1:8)
            if (l1 .ne. 0) then
!           CAS D'UN VECT_ASSE_GENE
                call jeveut(veasge//'.VALE', 'L', jvale)
                iadvec(i)=jvale
                idescf(i)=1
            else
!           CAS D'UN NUME_ORDRE
!           VERIF : LE NUME_ORDRE EST INFERIEUR AU NUME_ORDRE MAX
                if (inum .gt. neq) call u2mess('F', 'ALGORITH5_76')
                inumor(i)=inum
                idescf(i)=2
            endif
        else if (m1.ne.0) then
!         CAS D'UN COEF MULT
            coefm(i)=alpha
            if (l1 .ne. 0) then
!           CAS D'UN VECT_ASSE_GENE
                call jeveut(veasge//'.VALE', 'L', jvale)
                iadvec(i)=jvale
                idescf(i)=3
            else
!           CAS D'UN NUME_ORDRE
                if (inum .gt. neq) call u2mess('F', 'ALGORITH5_76')
                inumor(i)=inum
                idescf(i)=4
            endif
        else if (na.ne.0) then
!         CAS D'UN ACCELEROGRAMME
            nomfon(i) = facce(1:8)
            fonacc(i) = facce(1:8)
            call jeveuo(facce//'.PROL', 'L', lprol)
            nomfon(i+nexcit) = zk24(lprol)(1:8)
            fonacc(i+nexcit) = zk24(lprol)(1:8)
            if (l1 .ne. 0) then
!           CAS D'UN VECT_ASSE_GENE
                call jeveut(veasge//'.VALE', 'L', jvale)
                iadvec(i)=jvale
                idescf(i)=1
            else
!           CAS D'UN NUME_ORDRE
                if (inum .gt. neq) call u2mess('F', 'ALGORITH5_76')
                inumor(i)=inum
                idescf(i)=2
            endif
        endif
        if (n2 .ne. 0) then
            if (monmot(1) .eq. 'OUI') then
                nommot = 'OUI'
                call jeexin(nomres//'           .IPSD', iipsdl)
                if (iipsdl .eq. 0) then
                    call wkvect(nomres//'           .IPSD', 'G V R8', npsdel, jpsdel)
                else
                    call jeveuo(nomres//'           .IPSD', 'E', jpsdel)
                endif
!
                call getvid(' ', 'MODE_STAT', 1, iarg, 1,&
                            modsta, nbv)
                if (nbv .eq. 0) then
                    ier =ier+1
                    call u2mesg('E', 'ALGORITH13_46', 0, ' ', 0,&
                                0, 0, 0.d0)
                    goto 10
                endif
                call trmult(modsta, i, mailla, neq, iddeeq,&
                            zr(jpsdel+(i-1) *neq))
                call getvid('EXCIT', 'VITE', i, iarg, 1,&
                            fonvit(i), n4)
                fonct = fonvit(i)
                call jeveuo(fonct//'.PROL', 'L', lprol)
                fonvit(i+nexcit) = zk24(lprol)(1:8)
                call getvid('EXCIT', 'DEPL', i, iarg, 1,&
                            fondep(i), n5)
                fonct = fondep(i)(1:8)
                call jeveuo(fonct//'.PROL', 'L', lprol)
                fondep(i+nexcit) = zk24(lprol)(1:8)
            else
                ASSERT(.false.)
            endif
        endif
        if (n3 .ne. 0) then
            if (monmot(2) .eq. 'OUI') then
                nommot = 'OUI'
                call jeexin(nomres//'           .IPSD', iipsdl)
                if (iipsdl .eq. 0) then
                    call wkvect(nomres//'           .IPSD', 'G V R8', npsdel, jpsdel)
                else
                    call jeveuo(nomres//'           .IPSD', 'E', jpsdel)
                endif
                call getvid(' ', 'MODE_CORR', 1, iarg, 1,&
                            modcor, nbv)
                if (nbv .eq. 0) then
                    ier =ier+1
                    call u2mesg('E', 'ALGORITH13_47', 0, ' ', 0,&
                                0, 0, 0.d0)
                    goto 10
                endif
                call getvid('EXCIT', 'D_FONC_DT', i, iarg, 1,&
                            fonvit(i), n4)
                fonct = fonvit(i)(1:8)
                call jeveuo(fonct//'.PROL', 'L', lprol)
                fonvit(i+nexcit) = zk24(lprol)(1:8)
                call getvid('EXCIT', 'D_FONC_DT2', i, iarg, 1,&
                            fonacc(i), n5)
                fonct = fonacc(i)(1:8)
                call jeveuo(fonct//'.PROL', 'L', lprol)
                fonacc(i+nexcit) = zk24(lprol)(1:8)
                fondep(i) = nomfon(i)(1:8)
                fondep(i+nexcit) = nomfon(i+nexcit)(1:8)
!
                call rsexch('F', modcor, 'DEPL', i, chamno,&
                            iret)
                call jeveuo(chamno//'.VALE', 'L', imodco)
                do 30 ieq = 1, neq
                    zr(jpsdel+ieq-1+(i-1)*neq) = zr(imodco+ieq-1)
30              continue
                do 40 nm = 1, nbmode
                    coef = zr(iadvec(i)+nm-1)/riggen(nm)
                    call rsexch('F', basemo, 'DEPL', nm, chamn2,&
                                iret)
                    call jeveuo(chamn2//'.VALE', 'L', imod)
                    do 50 ieq = 1, neq
                        zr(jpsdel+ieq-1+(i-1)*neq) = zr(&
                                                     jpsdel+ieq-1+(i-1)* neq&
                                                     ) - coef*zr(imod+ieq-1&
                                                     )
50                  continue
                    call jelibe(chamn2//'.VALE')
40              continue
                call jelibe(chamno//'.VALE')
!
!           --- MISE A ZERO DES DDL DE LAGRANGE
                call zerlag('R', zr(jpsdel+(i-1)*neq), cbid, neq, zi( iddeeq))
!
            else
                ASSERT(.false.)
            endif
        endif
10  end do
!
!
! --- EXCITATIONS SOUS LE MOT-CLE EXCIT_RESU
!
! ON TRANSFORME CES EXCITATIONS SOUS LA FORME D'EXCTATIONS MODALES
! (MC NUME_ORDRE), ASSOCIEES A UNE FONCTION MULTIPLICATRICE
!
    call jeveuo(basemo//'           .ORDR', 'L', jordr)
    do 11 i = 1, nexcir
!
        call getvid('EXCIT_RESU', 'RESULTAT', i, iarg, 1,&
                    resu, l1)
        call getvr8('EXCIT_RESU', 'COEF_MULT', i, iarg, 1,&
                    alpha, m1)
! ----- NOMBRE DE PAS DE TEMPS DU RESULTAT
        call jelira(resu//'.DISC', 'LONMAX', ninst, k8bid)
        call jeveuo(resu//'.DISC', 'L', jinst)
! ----- EXCITATION STOCKEE DANS LE CHAMP DEPL (IDEM QUE DYNA_LINE_TRAN)
        call jeveuo(resu//'.DEPL', 'L', jdepl)
!
        ii = nexci+nbmode*(i-1)
        call codent(i, 'D0', ires)
!
        do 21 jmod = 1, nbmode
            call codent(jmod, 'D0', imo)
            nomfon(ii+jmod) = 'FON'//ires//imo
            nofk19 = nomfon(ii+jmod)
            call wkvect(nofk19//'.VALE', 'V V R8', 2*ninst, jvale)
            do 31 iinst = 1, ninst
                zr(jvale-1+iinst) = zr(jinst-1+iinst)
                zr(jvale-1+ninst+iinst) = alpha* zr( jdepl-1+nbmode*( iinst-1)+jmod)
31          continue
!
            call wkvect(nofk19//'.PROL', 'V V K24', 6, jprol)
            zk24(jprol-1+1)='FONCTION'
            zk24(jprol-1+2)='LIN LIN'
            zk24(jprol-1+3)='INST'
            zk24(jprol-1+4)='TOUTRESU'
            zk24(jprol-1+5)='EE'
            zk24(jprol-1+6)=nomfon(ii)
!
            nomfon(nexcit+ii+jmod) = zk24(jprol)(1:8)
!
            inumor(ii+jmod)=zi(jordr-1+jmod)
            idescf(ii+jmod)=2
!
21      continue
!
11  end do
!
    call jedema()
end subroutine
