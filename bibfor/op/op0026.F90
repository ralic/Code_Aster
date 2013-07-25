subroutine op0026()
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
    implicit none
#include "jeveux.h"
!
! ----------------------------------------------------------------------
!
!           O P E R A T E U R    C A L C U L
!           ================================
!
! ----------------------------------------------------------------------
!
!
#include "asterc/getres.h"
#include "asterc/getvid.h"
#include "asterc/getvis.h"
#include "asterc/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/chpver.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/diinst.h"
#include "asterfort/dismoi.h"
#include "asterfort/gcncon.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/knindi.h"
#include "asterfort/medime.h"
#include "asterfort/merimo.h"
#include "asterfort/mvnume.h"
#include "asterfort/nmch1p.h"
#include "asterfort/nmch2p.h"
#include "asterfort/nmcha0.h"
#include "asterfort/nmchai.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmdome.h"
#include "asterfort/nmdorc.h"
#include "asterfort/nmvcle.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/u2mesg.h"
#include "asterfort/vebtla.h"
#include "asterfort/vrcomp.h"
    integer :: nbpar, nblibl
    parameter       (nbpar=5,nblibl=6)
    character(len=19) :: nompar(nbpar), typpar(nbpar)
    character(len=24) :: vk(nbpar)
    character(len=24) :: knoobj(nblibl), vkk(nblibl), knotyp(nblibl)
    integer :: zsolal, zvalin
    parameter       (zsolal=17,zvalin=28)
    character(len=19) :: valinc(zvalin), solalg(zsolal)
!-----------------------------------------------------------------------
    integer :: n1, nbopt, iterat, numeor, i, ibid, nbpa
    integer :: niv, ifm, vi(1), jtbnp
    integer :: iret, nuord, long
    integer :: inoobj, inomsd, inuord, iinst, jnobj, jnosd
    integer :: jnuor, jrins, jlins, nbli, j, jtblp
    real(kind=8) :: instam, instap, vr(1)
    complex(kind=8) :: cbid
    character(len=2) :: codret
    character(len=8) :: result, table, tablu, k8b
    character(len=16) :: lopt(3), option
    character(len=19) :: lischa, k19bla
    character(len=19) :: linst
    character(len=24) :: modele, mate, carele, compor, carcri
    character(len=24) :: codere, ligrmo
    character(len=24) :: comref, k24bid
    character(len=19) :: nomtab
    character(len=19) :: commoi, complu, depplu
    character(len=19) :: depmoi, depdel, varplu, sigplu, varmoi, sigmoi
    character(len=19) :: mediri, merigi, vediri, vefint
    logical :: lmatr, lvect, lcomp, lrigi, ldiri
    logical :: tabret(0:10)
    integer :: fonact(100)
    integer :: iarg
!-----------------------------------------------------------------------
    data lischa     /'&&OP0026.LISCHA'/
    data carele     /'&&OP0026.CARELE'/
    data carcri     /'&&OP0026.CARCRI'/
    data comref     /'&&OP0026.COMREF'/
    data knoobj     /'MATR_ELEM               ',&
     &                 'SIEF_ELGA               ',&
     &                 'VARI_ELGA               ',&
     &                 'VECT_ELEM               ',&
     &                 'VEDIRI_EL               ',&
     &                 'CODE_RETOUR             '/
    data knotyp     /'MATR_ELEM_DEPL_R        ',&
     &                 'CHAM_ELEM               ',&
     &                 'CHAM_ELEM               ',&
     &                 'VECT_ELEM_DEPL_R        ',&
     &                 'VECT_ELEM_DEPL_R        ',&
     &                 'CHAM_ELEM               '/
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
!
! --- RECUPERATION DU NOM DE LA TABLE PRODUITE
!
    call getres(table, k24bid, k24bid)
!
! --- VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
!
    call nmchai('VALINC', 'LONMAX', long)
    call assert(long.eq.zvalin)
    call nmch1p(valinc)
!
! --- VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
!
    call nmchai('SOLALG', 'LONMAX', long)
    call assert(long.eq.zsolal)
    call nmch2p(solalg)
!
! --- RECUPERATION DES OPTIONS DEMANDEES
!
    call getvtx(' ', 'OPTION', 0, iarg, 3,&
                lopt, nbopt)
!
! --- RECUPERATION DU MODELE, DU MATERIAU, DES CHARGES
!
    nuord = 0
    modele = ' '
    k19bla = ' '
    do i = 1, 100
        fonact(i) = 0
    end do
    call nmdome(modele, mate, carele, lischa, result,&
                nuord)
    call dismoi('F', 'NOM_LIGREL', modele, 'MODELE', ibid,&
                ligrmo, iret)
!
! --- RECUPERATION DES DEPLACEMENTS
!
    call getvid(' ', 'DEPL', 0, iarg, 1,&
                depmoi, n1)
    call getvid(' ', 'INCR_DEPL', 0, iarg, 1,&
                depdel, n1)
    call nmcha0('VALINC', 'DEPMOI', depmoi, valinc)
    call nmcha0('SOLALG', 'DEPDEL', depdel, solalg)
!
! --- CALCUL DE DEPPLU
!
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
    call mvnume(depmoi, depdel, depplu)
!
! --- RECUPERATION DES CONTRAINTES
!
    call getvid(' ', 'SIGM', 0, iarg, 1,&
                sigmoi, n1)
    call chpver('F', sigmoi, 'ELGA', 'SIEF_R', iret)
    call nmcha0('VALINC', 'SIGMOI', sigmoi, valinc)
!
! --- RECUPERATION DES VARIABLES INTERNES
!
    call getvid(' ', 'VARI', 0, iarg, 1,&
                varmoi, n1)
    call chpver('F', varmoi, 'ELGA', 'VARI_R', iret)
    call nmcha0('VALINC', 'VARMOI', varmoi, valinc)
!
! --- RECUPERATION DU COMPORTEMENT
!
    call nmdorc(modele, compor, carcri)
!
! --- RECUPERATION DU NUMERO D'ORDRE ET DE L'INSTANT COURANTS
!
    linst = ' '
    call getvis('INCREMENT', 'NUME_ORDRE', 1, iarg, 1,&
                numeor, n1)
    call getvid('INCREMENT', 'LIST_INST', 1, iarg, 1,&
                linst, n1)
    instap = diinst(linst,numeor-1)
    instam = diinst(linst,numeor)
!
! --- LECTURE DES VARIABLES DE COMMANDE A L'INSTANT COURANT
!
    call nmchex(valinc, 'VALINC', 'COMMOI', commoi)
    call nmchex(valinc, 'VALINC', 'COMPLU', complu)
    call nmvcle(modele, mate, carele, lischa, instap,&
                complu, codret)
    call nmvcle(modele, mate, carele, lischa, instam,&
                commoi, codret)
!
! --- CREATION DU CHAMP DES VARIABLES DE COMMANDE DE REFERENCE
!
    call nmvcre(modele, mate, carele, comref)
!
! --- VERIFICATION DU NOMBRE DE VARIABLES INTERNES
!
    call jeexin(compor(1:19)//'.CESD', iret)
    if (iret .gt. 0) call vrcomp(' ', compor, varmoi, ligrmo)
!
! --- GENERATION DU NOM DES SD
!
    call gcncon('_', sigplu)
    call gcncon('_', varplu)
    call gcncon('_', merigi)
    call gcncon('_', vefint)
    call gcncon('_', mediri)
    call gcncon('_', vediri)
    call gcncon('_', codere)
!
! --- RE-DEFINITION DES NOMS DES SD
!
    call nmcha0('VALINC', 'SIGPLU', sigplu, valinc)
    call nmcha0('VALINC', 'VARPLU', varplu, valinc)
!
! --- TYPE DE QUANTITES CALCULEES
!
    lcomp = .false.
    lmatr = .false.
    lvect = .false.
    if (knindi(16,'COMPORTEMENT',lopt,nbopt) .gt. 0) then
        lcomp = .true.
    endif
    if (knindi(16,'MATR_TANG_ELEM',lopt,nbopt) .gt. 0) then
        lmatr = .true.
    endif
    if (knindi(16,'FORC_INT_ELEM',lopt,nbopt) .gt. 0) then
        lvect = .true.
    endif
!
! --- ROUTINES DE CALCUL APPELLEES
!
    lrigi = .false.
    ldiri = .false.
    if (lcomp .or. lmatr .or. lvect) then
        lrigi = .true.
    endif
    if (lmatr) then
        ldiri = .true.
        call assert(lrigi)
    endif
!
! --- OPTIONS DE CALCUL
!
    option = ' '
    if (lmatr) then
        option = 'FULL_MECA'
    else
        option = 'RAPH_MECA'
    endif
!
! --- CALCUL DE LA CONTRIBUTION DES DDL PHYSIQUES (MERIGI ET VEFINT)
!
    if (lrigi) then
        iterat=1
        call merimo('G', modele, carele, mate, comref,&
                    compor, carcri, iterat, fonact, k19bla,&
                    valinc, solalg, merigi, vefint, option,&
                    tabret, codere)
    endif
!
! --- CALCUL DE LA CONTRIBUTION DES "LAGRANGE"
!
    if (ldiri) then
        call medime('G', 'CUMU', modele, lischa, merigi)
        call vebtla('G', modele, mate, carele, depplu,&
                    lischa, vediri)
    endif
!
! --- ECRITURE DES RESULTATS DANS LA TABLE
!
    nompar(1) = 'NOM_OBJET'
    typpar(1) = 'K16'
!
    nompar(2) = 'TYPE_OBJET'
    typpar(2) = 'K16'
!
    nompar(3) = 'NOM_SD'
    typpar(3) = 'K24'
!
    nompar(4) = 'NUME_ORDRE'
    typpar(4) = 'I'
!
    nompar(5) = 'INST'
    typpar(5) = 'R8'
!
    vr(1)=instap
    vi(1)=numeor
!
    vkk(1)=merigi
    vkk(2)=sigplu
    vkk(3)=varplu
    vkk(4)=vefint
    vkk(5)=vediri
    vkk(6)=codere
!
    call getvid(' ', 'TABLE', 0, iarg, 0,&
                k8b, n1)
!
!     ---------------------------------------------
!     CAS 1 - ON CREE UNE NOUVELLE TABLE CONTAINER
!     ---------------------------------------------
    if (n1 .eq. 0) then
!
        call detrsd('TABLE_CONTAINER', table)
        call tbcrsd(table, 'G')
!
        call tbajpa(table, nbpar, nompar, typpar)
        vk(1)=knoobj(1)
        vk(2)=knotyp(1)
        vk(3)=vkk(1)
        call tbajli(table, nbpar, nompar, vi, vr,&
                    cbid, vk, 0)
        vk(1)=knoobj(2)
        vk(2)=knotyp(2)
        vk(3)=vkk(2)
        call tbajli(table, nbpar, nompar, vi, vr,&
                    cbid, vk, 0)
        vk(1)=knoobj(3)
        vk(2)=knotyp(3)
        vk(3)=vkk(3)
        call tbajli(table, nbpar, nompar, vi, vr,&
                    cbid, vk, 0)
        vk(1)=knoobj(4)
        vk(2)=knotyp(4)
        vk(3)=vkk(4)
        call tbajli(table, nbpar, nompar, vi, vr,&
                    cbid, vk, 0)
        vk(1)=knoobj(5)
        vk(2)=knotyp(5)
        vk(3)=vkk(5)
        call tbajli(table, nbpar, nompar, vi, vr,&
                    cbid, vk, 0)
        vk(1)=knoobj(6)
        vk(2)=knotyp(6)
        vk(3)=vkk(6)
        call tbajli(table, nbpar, nompar, vi, vr,&
                    cbid, vk, 0)
!
    else
!
!     -----------------------------------------------------
!     CAS 2 - ON ENRICHIT UNE TABLE CONTAINER  OU
!             ON EN CREE UNE NOUVELLE A PARTIR D'UNE AUTRE
!     ----------------------------------------------------
!
        call getvid(' ', 'TABLE', 0, iarg, 1,&
                    tablu, n1)
!
        if (tablu .ne. table) then
            call detrsd('TABLE_CONTAINER', table)
            call copisd('TABLE', 'G', tablu, table)
        endif
!
        nomtab=table
        call jeveuo(nomtab//'.TBNP', 'L', jtbnp)
        nbpa=zi(jtbnp)
        nbli=zi(jtbnp+1)
!
!       RECHERCHE DES PARAMETRES CONCERNES DANS LA TABLE FOURNIE
        inoobj=0
        inomsd=0
        inuord=0
        iinst=0
        call jeveuo(nomtab//'.TBLP', 'L', jtblp)
        do i = 1, nbpa
            if (zk24(jtblp+(i-1)*4)(1:9) .eq. 'NOM_OBJET') then
                inoobj=i
            else if (zk24(jtblp+(i-1)*4)(1:6).eq.'NOM_SD') then
                inomsd=i
            else if (zk24(jtblp+(i-1)*4)(1:10).eq.'NUME_ORDRE') then
                inuord=i
            else if (zk24(jtblp+(i-1)*4)(1:4).eq.'INST') then
                iinst=i
            endif
        enddo
!
        call assert(inoobj.ne.0)
        call assert(inomsd.ne.0)
        call assert(inuord.ne.0)
        call assert(iinst .ne.0)
!
!       RECUPERATION DES POINTEURS POUR LIRE ET MODIFIER LA TABLE
        call jeveuo(zk24(jtblp+(inoobj-1)*4+2), 'L', jnobj)
        call jeveuo(zk24(jtblp+(inomsd-1)*4+2), 'E', jnosd)
        call jeveuo(zk24(jtblp+(inuord-1)*4+2), 'E', jnuor)
        call jeveuo(zk24(jtblp+(iinst -1)*4+2), 'E', jrins)
        call jeveuo(zk24(jtblp+(iinst -1)*4+3), 'L', jlins)
!
!       POUR LES NBLIBL(=6) NOM_OBJET DE CALCUL
        do i = 1, nblibl
!         ON PARCOURT LES LIGNES DE LA TABLE POUR:
            do j = 1, nbli
!          - IDENTIFIER LES LIGNES OU L'ON TROUVE LE NOM_OBJET DE CALCUL
                if (zk16(jnobj+j-1) .eq. knoobj(i)(1:16)) then
                    if (zi(jlins+j-1) .eq. 1) then
!          - LIRE L'INSTANT PRESENT DANS LA TABLE:
!            SI CELUI-CI EST IDENTIQUE A L'INSTANT DU NOM_SD A STOCKER
                        if (zr(jrins+j-1) .eq. vr(1)) then
!            ALORS, ON ECRASE LE CONCEPT NOM_SD ET ON LE REMPLACE PAR
!            LE NOUVEAU (ON MET A JOUR AUSSI NUME_ORDRE ET INST)
                            vk(1)=knoobj(i)
                            vk(2)=tablu
                            call u2mesg('A', 'TABLE0_16', 2, vk, 0,&
                                        ibid, 1, vr)
                            call jedetr(zk24(jnosd+j-1))
                            zk24(jnosd+j-1)=vkk(i)
                            zi(jnuor+j-1)=vi(1)
                            zr(jrins+j-1)=vr(1)
                            goto 15
                        endif
                    endif
                endif
             enddo
!         SI LE NOM_OBJET ET L'INSTANT N'ONT PAS ETE TROUVES, ALORS ON
!         AJOUTE UNE NOUVELLE LIGNE A LA TABLE:
            vk(1)=knoobj(i)
            vk(2)=knotyp(i)
            vk(3)=vkk(i)
            call tbajli(table, nbpar, nompar, vi, vr,&
                        cbid, vk, 0)
15          continue
        enddo
!
    endif
!
!
    call jedema()
!
end subroutine
