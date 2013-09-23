subroutine calimc(chargz)
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
!
    implicit none
!
!       CALIMC -- TRAITEMENT DU MOT FACTEUR LIAISON_INTERF
!
!      TRAITEMENT DU MOT FACTEUR LIAISON_INTERF DE AFFE_CHAR_MECA
!      CE MOT FACTEUR PERMET DE DEFINIR UNE RELATION LINEAIRE ENTRE
!      LES DDLS PHYSIQUES DE L INTERFACE DYNAMIQUE D UN MACRO-ELEMENT
!      ET LES DDLS VIRTUELS DE L OBJET LINO
!
! -------------------------------------------------------
!  CHARGE        - IN    - K8   - : NOM DE LA SD CHARGE
!                - JXVAR -      -   LA  CHARGE EST ENRICHIE
!                                   DE LA RELATION LINEAIRE DECRITE
!                                   CI-DESSUS.
! -------------------------------------------------------
!
!.========================= DEBUT DES DECLARATIONS ====================
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/aflrch.h"
#include "asterfort/afrela.h"
#include "asterfort/assert.h"
#include "asterfort/copmod.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsorac.h"
#include "asterfort/wkvect.h"
!
!
! -----  ARGUMENTS
    character(len=*) :: chargz
!      CHARACTER*8 NOMA
! ------ VARIABLES LOCALES
    complex(kind=8) :: betac, cbid
    character(len=2) :: typlag
    character(len=4) :: typval, typcoe
    character(len=8) :: nomcmp, nomnoe, betaf, nmcmp2, nmnoe2
    character(len=6) :: typlia
    character(len=8) :: charge
    character(len=16) :: motfac
    character(len=19) :: lisrel
    character(len=14) :: numddl
    character(len=8) :: k8b, basemo, mailla, liscmp(6), macrel, lintf, nomnol
    character(len=8) :: nogdsi
    character(len=24) :: numedd, nprno
    character(len=3) :: ttran
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, i2, i3, iaconx, iadref, iaprno
    integer :: ibid, icmp, icmp2, idbase, idcoec, idcoer, idddl
    integer :: iddl, iddl2, idimen, idirec, idnoeu, ierd, ii
    integer :: imod, imod2, inoe, iocc, iret, j, j2
    integer :: j3, jj, jncmpd, jncmpi, k, lldef, n2
    integer :: nbec, nbmdef, nbmdyn, nbmode(1), nbnde2, nbndef, nbndyn
    integer :: nbnoe, nbntot, nbterm, nec, nec2, neq, nliai, nueq
    integer :: nmc
    real(kind=8) :: beta, rbid, vale, zero
!-----------------------------------------------------------------------
    data liscmp   /'DX      ','DY      ','DZ      ',&
     &               'DRX     ','DRY     ','DRZ     '/
!     ------------------------------------------------------------------
!
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
    call jemarq()
!
    motfac = 'LIAISON_INTERF'
!
    call getfac(motfac, nliai)
    if (nliai .eq. 0) goto 40
!
! --- INITIALISATIONS :
!     ---------------
    zero = 0.0d0
! --- BETA, BETAC ET BETAF SONT LES VALEURS DU SECOND MEMBRE DE LA
! --- RELATION LINEAIRE SUIVANT QUE C'EST UN REEL, UN COMPLEXE OU
! --- UNE FONCTION, DANS NOTRE CAS C'EST UN REEL
!
    beta = zero
    betac = (0.0d0,0.0d0)
    betaf = '&FOZERO'
!
    charge = chargz
!
! --- TYPE DES VALEURS AU SECOND MEMBRE DE LA RELATION
!
    typval = 'REEL'
!
! --- TYPE DES VALEURS DES COEFFICIENTS
!
    typcoe = 'REEL'
!
! --- NOM DE LA LISTE_RELA
!
    lisrel = '&CALIMC.RLLISTE'
!
! --- BOUCLE SUR LES OCCURENCES DU MOT-FACTEUR LIAISON_MACREL :
!     -------------------------------------------------------
    do 30 iocc = 1, nliai
!
! ---   ON REGARDE SI LES MULTIPLICATEURS DE LAGRANGE SONT A METTRE
! ---   APRES LES NOEUDS PHYSIQUES LIES PAR LA RELATION DANS LA MATRICE
! ---   ASSEMBLEE :
! ---   SI OUI TYPLAG = '22'
! ---   SI NON TYPLAG = '12'
!
!        CALL GETVTX(MOTFAC,'NUME_LAGR',IOCC,IARG,1,POSLAG,IBID)
!        IF (POSLAG.EQ.'APRES') THEN
!          TYPLAG = '22'
!        ELSE
!          TYPLAG = '12'
!        ENDIF
        typlag = '12'
        call getvid(motfac, 'MACR_ELEM_DYNA', iocc=iocc, scal=macrel, nbret=nmc)
        call jeveuo(macrel//'.MAEL_REFE', 'L', iadref)
        basemo = zk24(iadref)
        call rsorac(basemo, 'LONUTI', 0, rbid, k8b,&
                    cbid, rbid, k8b, nbmode, 1,&
                    ibid)
        call dismoi('F', 'NUME_DDL', basemo, 'RESU_DYNA', ibid,&
                    numedd, iret)
        call dismoi('F', 'NOM_MAILLA', numedd(1:14), 'NUME_DDL', ibid,&
                    mailla, iret)
        call dismoi('F', 'REF_INTD_PREM', basemo, 'RESU_DYNA', ibid,&
                    lintf, iret)
! On recupere le nbre de noeuds presents dans interf_dyna
        call jelira(jexnum(lintf//'.IDC_LINO', 1), 'LONMAX', nbnoe)
! On recupere la liste des noeuds presents dans interf_dyna
        call jeveuo(lintf//'.IDC_DEFO', 'L', lldef)
! On recupere le nbre de modes statiques dans la base
        call dismoi('F', 'NB_MODES_STA', basemo, 'RESULTAT', nbmdef,&
                    k8b, ierd)
        call jelira(macrel//'.LINO', 'LONMAX', nbntot)
        nbmdyn = nbmode(1)-nbmdef
        nec = nbmode(1)/nbntot
        nbndyn = nbmdyn/nec
        nbndef = nbntot-nbndyn
        nbnde2 = nbmdef/nec
        ASSERT(nbndef.eq.nbnde2)
!       CREATION DU TABLEAU NOEUD-COMPOSANTE ASSOCIES AUX MODES
        call wkvect('&&CALIMC.NCMPSD', 'V V K8', 2*nbmdef, jncmpd)
        call jeveuo(macrel//'.LINO', 'L', iaconx)
        do 21 i = 1, nbndef
            i2 = i+nbndyn
            call jenuno(jexnum(mailla//'.NOMNOE', zi(iaconx+i2-1)), nomnol)
            do 22 j = 1, nec
                zk8(jncmpd+2*nec*(i-1)+2*j-2) = nomnol
                zk8(jncmpd+2*nec*(i-1)+2*j-1) = liscmp(j)
22          continue
21      continue
        call wkvect('&&CALIMC.NCMPIN', 'V V K8', 2*nbnoe*nec, jncmpi)
        do 23 i = 1, nbnoe
            call jenuno(jexnum(mailla//'.NOMNOE', zi(lldef+i-1)), nomnol)
            do 24 j = 1, nec
                zk8(jncmpi+2*nec*(i-1)+2*j-2) = nomnol
                zk8(jncmpi+2*nec*(i-1)+2*j-1) = liscmp(j)
24          continue
23      continue
        numddl = numedd(1:14)
        call dismoi('F', 'NB_EQUA', numddl, 'NUME_DDL', neq,&
                    k8b, iret)
        call wkvect('&&CALIMC.BASE', 'V V R', nbmode(1)*neq, idbase)
        call copmod(basemo, 'DEPL', neq, numddl, nbmode(1),&
                    'R', zr(idbase), cbid)
        call dismoi('F', 'NOM_GD', numddl, 'NUME_DDL', ibid,&
                    nogdsi, ierd)
!        NOGDSI = 'DEPL_R'
        call dismoi('F', 'NB_EC', nogdsi, 'GRANDEUR', nbec,&
                    k8b, ierd)
        nprno = numddl//'.NUME.PRNO'
        call jeveuo(jexnum(nprno, 1), 'L', iaprno)
!
        call getvtx(motfac, 'TYPE_LIAISON', iocc=iocc, scal=typlia, nbret=n2)
!
        if (typlia .eq. 'RIGIDE') then
            nbterm = nbmdef+1
        else
            nbterm = nbmdef+nec*nbnoe
        endif
!
! ---   CREATION DES TABLEAUX DE TRAVAIL NECESSAIRES A L'AFFECTATION
! ---   DE LA LISTE_RELA
!       ----------------
! ---     VECTEUR DU NOM DES NOEUDS
        call wkvect('&&CALIMC.LISNO', 'V V K8', nbterm, idnoeu)
! ---     VECTEUR DU NOM DES DDLS
        call wkvect('&&CALIMC.LISDDL', 'V V K8', nbterm, idddl)
! ---      VECTEUR DES COEFFICIENTS REELS
        call wkvect('&&CALIMC.COER', 'V V R', nbterm, idcoer)
! ---     VECTEUR DES COEFFICIENTS COMPLEXES
        call wkvect('&&CALIMC.COEC', 'V V C', nbterm, idcoec)
! ---     VECTEUR DES DIRECTIONS DES DDLS A CONTRAINDRE
        call wkvect('&&CALIMC.DIRECT', 'V V R', 3*nbterm, idirec)
! ---     VECTEUR DES DIMENSIONS DE CES DIRECTIONS
        call wkvect('&&CALIMC.DIME', 'V V I', nbterm, idimen)
!
!
! ---   AFFECTATION DES TABLEAUX DE TRAVAIL :
!       -----------------------------------
!
! ---   BOUCLE SUR LES DDL D'INTERFACE DU MACRO-ELEMENT
!        CALL GETVTX(MOTFAC, 'SANS_ROTA' , IOCC,IARG,1, TTRAN, N2 )
        ttran = 'NON'
        if (ttran .eq. 'OUI') then
            nec2 = 3
        else
            nec2 = nec
        endif
        if (typlia .ne. 'RIGIDE') goto 101
!
!       CAS RIGIDE
!
        do 25 i = 1, nbnoe
            do 26 j = 1, nec2
                k = 0
                nomnoe = zk8(jncmpi+2*nec*(i-1)+2*j-2)
                nomcmp = zk8(jncmpi+2*nec*(i-1)+2*j-1)
                call jenonu(jexnom(mailla//'.NOMNOE', nomnoe), inoe)
                if (nomcmp .eq. 'DX') icmp = 1
                if (nomcmp .eq. 'DY') icmp = 2
                if (nomcmp .eq. 'DZ') icmp = 3
                if (nomcmp .eq. 'DRX') icmp = 4
                if (nomcmp .eq. 'DRY') icmp = 5
                if (nomcmp .eq. 'DRZ') icmp = 6
                iddl = zi(iaprno-1+(nbec+2)*(inoe-1)+1)
                nueq = zi(iaprno-1+(nbec+2)*(inoe-1)+2)
                if (icmp .gt. nueq) goto 26
                do 27 ii = 1, nbndef
                    do 28 jj = 1, nec
                        k = k + 1
                        nomnoe = zk8(jncmpd+2*nec*(ii-1)+2*jj-2)
                        nomcmp = zk8(jncmpd+2*nec*(ii-1)+2*jj-1)
                        imod = nbmdyn+(ii-1)*nec+jj
                        vale = zr(idbase+(imod-1)*neq+iddl-1+icmp-1)
                        zk8(idnoeu+k-1) = nomnoe
                        zk8(idddl+k-1) = nomcmp
                        zr(idcoer+k-1) = vale
28                  continue
27              continue
                k = nbterm
                nomnoe = zk8(jncmpi+2*nec*(i-1)+2*j-2)
                nomcmp = zk8(jncmpi+2*nec*(i-1)+2*j-1)
                zk8(idnoeu+k-1) = nomnoe
                zk8(idddl+k-1) = nomcmp
                zr(idcoer+k-1) = -1.0d0
!
! ---   AFFECTATION DE LA RELATION A LA LISTE_RELA  :
!       ------------------------------------------
                call afrela(zr(idcoer), zc(idcoec), zk8(idddl), zk8( idnoeu), zi(idimen),&
                            zr(idirec), nbterm, beta, betac, betaf,&
                            typcoe, typval, typlag, 0.d0, lisrel)
!
26          continue
25      continue
        goto 102
101      continue
!
!       CAS SOUPLE
!
        do 31 i = 1, nbndef
            do 32 j = 1, nec
                k = 0
                imod = nbmdyn+(i-1)*nec+j
                do 35 i2 = 1, nbnoe
                    nomnoe = zk8(jncmpi+2*nec*(i2-1))
                    call jenonu(jexnom(mailla//'.NOMNOE', nomnoe), inoe)
                    iddl = zi(iaprno-1+(nbec+2)*(inoe-1)+1)
                    do 36 j2 = 1, nec
                        k = k + 1
                        nomcmp = zk8(jncmpi+2*nec*(i2-1)+2*j2-1)
                        if (nomcmp .eq. 'DX') icmp = 1
                        if (nomcmp .eq. 'DY') icmp = 2
                        if (nomcmp .eq. 'DZ') icmp = 3
                        if (nomcmp .eq. 'DRX') icmp = 4
                        if (nomcmp .eq. 'DRY') icmp = 5
                        if (nomcmp .eq. 'DRZ') icmp = 6
                        zk8(idnoeu+k-1) = nomnoe
                        zk8(idddl+k-1) = nomcmp
                        zr(idcoer+k-1) = -zr(idbase+(imod-1)*neq+iddl- 1+icmp-1 )
36                  continue
35              continue
                do 37 ii = 1, nbndef
                    nomnoe = zk8(jncmpd+2*nec*(ii-1))
                    do 38 jj = 1, nec
                        k = k + 1
                        nomcmp = zk8(jncmpd+2*nec*(ii-1)+2*jj-1)
                        imod2 = nbmdyn+(ii-1)*nec+jj
                        vale = zero
                        do 33 i3 = 1, nbnoe
                            nmnoe2 = zk8(jncmpi+2*nec*(i3-1))
                            call jenonu(jexnom(mailla//'.NOMNOE', nmnoe2), inoe)
                            iddl2 = zi(iaprno-1+(nbec+2)*(inoe-1)+1)
                            do 34 j3 = 1, nec
                                nmcmp2 = zk8(jncmpi+2*nec*(i3-1)+2*j3- 1)
                                if (nmcmp2 .eq. 'DX') icmp2 = 1
                                if (nmcmp2 .eq. 'DY') icmp2 = 2
                                if (nmcmp2 .eq. 'DZ') icmp2 = 3
                                if (nmcmp2 .eq. 'DRX') icmp2 = 4
                                if (nmcmp2 .eq. 'DRY') icmp2 = 5
                                if (nmcmp2 .eq. 'DRZ') icmp2 = 6
                                vale = vale + zr(&
                                       idbase+(imod-1)*neq+ iddl2-1+icmp2-1)* zr(idbase+(imod2-1)&
                                       &* neq+iddl2-1+icmp2-1&
                                       )
34                          continue
33                      continue
                        zk8(idnoeu+k-1) = nomnoe
                        zk8(idddl+k-1) = nomcmp
                        zr(idcoer+k-1) = vale
38                  continue
37              continue
! ---   AFFECTATION DE LA RELATION A LA LISTE_RELA  :
!       ------------------------------------------
                call afrela(zr(idcoer), zc(idcoec), zk8(idddl), zk8( idnoeu), zi(idimen),&
                            zr(idirec), nbterm, beta, betac, betaf,&
                            typcoe, typval, typlag, 0.d0, lisrel)
32          continue
31      continue
!
102      continue
! ---   MENAGE :
!       ------
        call jedetr('&&CALIMC.LISNO')
        call jedetr('&&CALIMC.LISDDL')
        call jedetr('&&CALIMC.COER')
        call jedetr('&&CALIMC.COEC')
        call jedetr('&&CALIMC.DIRECT')
        call jedetr('&&CALIMC.DIME')
        call jedetr('&&CALIMC.NCMPSD')
        call jedetr('&&CALIMC.NCMPIN')
        call jedetr('&&CALIMC.BASE')
!
30  continue
!
! --- AFFECTATION DE LA LISTE_RELA A LA CHARGE :
!     ----------------------------------------
    call aflrch(lisrel, charge)
!
! --- MENAGE :
!     ------
    call jedetr(lisrel)
!
40  continue
!
    call jedema()
!.============================ FIN DE LA ROUTINE ======================
end subroutine
