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
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
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
    integer :: ibid, icmp, icmp2, idbase
    integer :: iddl, iddl2,    ii
    integer :: imod, imod2, inoe, iocc, j, j2
    integer :: j3, jj,   k, lldef, n2
    integer :: nbec, nbmdef, nbmdyn, nbmode(1), nbnde2, nbndef, nbndyn
    integer :: nbnoe, nbntot, nbterm, nec, nec2, neq, nliai, nueq
    integer :: nmc
    real(kind=8) :: beta, rbid, vale, zero
    complex(kind=8), pointer :: coec(:) => null()
    real(kind=8), pointer :: coer(:) => null()
    integer, pointer :: dime(:) => null()
    real(kind=8), pointer :: direct(:) => null()
    character(len=8), pointer :: lisddl(:) => null()
    character(len=8), pointer :: lisno(:) => null()
    character(len=8), pointer :: ncmpin(:) => null()
    character(len=8), pointer :: ncmpsd(:) => null()
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
    do iocc = 1, nliai
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
        call dismoi('NUME_DDL', basemo, 'RESU_DYNA', repk=numedd)
        call dismoi('NOM_MAILLA', numedd(1:14), 'NUME_DDL', repk=mailla)
        call dismoi('REF_INTD_PREM', basemo, 'RESU_DYNA', repk=lintf)
! On recupere le nbre de noeuds presents dans interf_dyna
        call jelira(jexnum(lintf//'.IDC_LINO', 1), 'LONMAX', nbnoe)
! On recupere la liste des noeuds presents dans interf_dyna
        call jeveuo(lintf//'.IDC_DEFO', 'L', lldef)
! On recupere le nbre de modes statiques dans la base
        call dismoi('NB_MODES_STA', basemo, 'RESULTAT', repi=nbmdef)
        call jelira(macrel//'.LINO', 'LONMAX', nbntot)
        nbmdyn = nbmode(1)-nbmdef
        nec = nbmode(1)/nbntot
        nbndyn = nbmdyn/nec
        nbndef = nbntot-nbndyn
        nbnde2 = nbmdef/nec
        ASSERT(nbndef.eq.nbnde2)
!       CREATION DU TABLEAU NOEUD-COMPOSANTE ASSOCIES AUX MODES
        AS_ALLOCATE(vk8=ncmpsd, size=2*nbmdef)
        call jeveuo(macrel//'.LINO', 'L', iaconx)
        do i = 1, nbndef
            i2 = i+nbndyn
            call jenuno(jexnum(mailla//'.NOMNOE', zi(iaconx+i2-1)), nomnol)
            do j = 1, nec
                ncmpsd(1+2*nec*(i-1)+2*j-2) = nomnol
                ncmpsd(1+2*nec*(i-1)+2*j-1) = liscmp(j)
            end do
        end do
        AS_ALLOCATE(vk8=ncmpin, size=2*nbnoe*nec)
        do i = 1, nbnoe
            call jenuno(jexnum(mailla//'.NOMNOE', zi(lldef+i-1)), nomnol)
            do j = 1, nec
                ncmpin(1+2*nec*(i-1)+2*j-2) = nomnol
                ncmpin(1+2*nec*(i-1)+2*j-1) = liscmp(j)
            end do
        end do
        numddl = numedd(1:14)
        call dismoi('NB_EQUA', numddl, 'NUME_DDL', repi=neq)
        call wkvect('&&CALIMC.BASE', 'V V R', nbmode(1)*neq, idbase)
        call copmod(basemo, numer=numddl, bmodr=zr(idbase))
        call dismoi('NOM_GD', numddl, 'NUME_DDL', repk=nogdsi)
!        NOGDSI = 'DEPL_R'
        call dismoi('NB_EC', nogdsi, 'GRANDEUR', repi=nbec)
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
        AS_ALLOCATE(vk8=lisno, size=nbterm)
! ---     VECTEUR DU NOM DES DDLS
        AS_ALLOCATE(vk8=lisddl, size=nbterm)
! ---      VECTEUR DES COEFFICIENTS REELS
        AS_ALLOCATE(vr=coer, size=nbterm)
! ---     VECTEUR DES COEFFICIENTS COMPLEXES
        AS_ALLOCATE(vc=coec, size=nbterm)
! ---     VECTEUR DES DIRECTIONS DES DDLS A CONTRAINDRE
        AS_ALLOCATE(vr=direct, size=3*nbterm)
! ---     VECTEUR DES DIMENSIONS DE CES DIRECTIONS
        AS_ALLOCATE(vi=dime, size=nbterm)
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
        do i = 1, nbnoe
            do j = 1, nec2
                k = 0
                nomnoe = ncmpin(1+2*nec*(i-1)+2*j-2)
                nomcmp = ncmpin(1+2*nec*(i-1)+2*j-1)
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
                do ii = 1, nbndef
                    do jj = 1, nec
                        k = k + 1
                        nomnoe = ncmpsd(1+2*nec*(ii-1)+2*jj-2)
                        nomcmp = ncmpsd(1+2*nec*(ii-1)+2*jj-1)
                        imod = nbmdyn+(ii-1)*nec+jj
                        vale = zr(idbase+(imod-1)*neq+iddl-1+icmp-1)
                        lisno(k) = nomnoe
                        lisddl(k) = nomcmp
                        coer(k) = vale
                    end do
                end do
                k = nbterm
                nomnoe = ncmpin(1+2*nec*(i-1)+2*j-2)
                nomcmp = ncmpin(1+2*nec*(i-1)+2*j-1)
                lisno(k) = nomnoe
                lisddl(k) = nomcmp
                coer(k) = -1.0d0
!
! ---   AFFECTATION DE LA RELATION A LA LISTE_RELA  :
!       ------------------------------------------
                call afrela(coer, coec, lisddl, lisno, dime,&
                            direct, nbterm, beta, betac, betaf,&
                            typcoe, typval, typlag, 0.d0, lisrel)
!
 26             continue
            end do
        end do
        goto 102
101     continue
!
!       CAS SOUPLE
!
        do i = 1, nbndef
            do j = 1, nec
                k = 0
                imod = nbmdyn+(i-1)*nec+j
                do i2 = 1, nbnoe
                    nomnoe = ncmpin(1+2*nec*(i2-1))
                    call jenonu(jexnom(mailla//'.NOMNOE', nomnoe), inoe)
                    iddl = zi(iaprno-1+(nbec+2)*(inoe-1)+1)
                    do j2 = 1, nec
                        k = k + 1
                        nomcmp = ncmpin(1+2*nec*(i2-1)+2*j2-1)
                        if (nomcmp .eq. 'DX') icmp = 1
                        if (nomcmp .eq. 'DY') icmp = 2
                        if (nomcmp .eq. 'DZ') icmp = 3
                        if (nomcmp .eq. 'DRX') icmp = 4
                        if (nomcmp .eq. 'DRY') icmp = 5
                        if (nomcmp .eq. 'DRZ') icmp = 6
                        lisno(k) = nomnoe
                        lisddl(k) = nomcmp
                        coer(k) = -zr(idbase+(imod-1)*neq+iddl- 1+icmp-1 )
                    end do
                end do
                do ii = 1, nbndef
                    nomnoe = ncmpsd(1+2*nec*(ii-1))
                    do jj = 1, nec
                        k = k + 1
                        nomcmp = ncmpsd(1+2*nec*(ii-1)+2*jj-1)
                        imod2 = nbmdyn+(ii-1)*nec+jj
                        vale = zero
                        do i3 = 1, nbnoe
                            nmnoe2 = ncmpin(1+2*nec*(i3-1))
                            call jenonu(jexnom(mailla//'.NOMNOE', nmnoe2), inoe)
                            iddl2 = zi(iaprno-1+(nbec+2)*(inoe-1)+1)
                            do j3 = 1, nec
                                nmcmp2 = ncmpin(1+2*nec*(i3-1)+2*j3- 1)
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
                            end do
                        end do
                        lisno(k) = nomnoe
                        lisddl(k) = nomcmp
                        coer(k) = vale
                    end do
                end do
! ---   AFFECTATION DE LA RELATION A LA LISTE_RELA  :
!       ------------------------------------------
                call afrela(coer, coec, lisddl, lisno, dime,&
                            direct, nbterm, beta, betac, betaf,&
                            typcoe, typval, typlag, 0.d0, lisrel)
            end do
        end do
!
102     continue
! ---   MENAGE :
!       ------
        AS_DEALLOCATE(vk8=lisno)
        AS_DEALLOCATE(vk8=lisddl)
        AS_DEALLOCATE(vr=coer)
        AS_DEALLOCATE(vc=coec)
        AS_DEALLOCATE(vr=direct)
        AS_DEALLOCATE(vi=dime)
        AS_DEALLOCATE(vk8=ncmpsd)
        AS_DEALLOCATE(vk8=ncmpin)
        call jedetr('&&CALIMC.BASE')
!
    end do
!
! --- AFFECTATION DE LA LISTE_RELA A LA CHARGE :
!     ----------------------------------------
    call aflrch(lisrel, charge)
!
! --- MENAGE :
!     ------
    call jedetr(lisrel)
!
 40 continue
!
    call jedema()
!.============================ FIN DE LA ROUTINE ======================
end subroutine
