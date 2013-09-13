subroutine op0060()
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
!
! ----------------------------------------------------------------------
!
!  COMMANDE DYNA_LINE_HARM
!
!  CALCUL DYNAMIQUE HARMONIQUE POUR UN SYSTEME CONSERVATIF
!  OU DISSIPATIF Y COMPRIS LES SYSTEMES COUPLES FLUIDE-STRUCTURE
!
!
!
!
!
#include "jeveux.h"
#include "asterc/etausr.h"
#include "asterc/gcucon.h"
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterc/r8depi.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/cresol.h"
#include "asterfort/dismoi.h"
#include "asterfort/dy2mbr.h"
#include "asterfort/dydome.h"
#include "asterfort/dyexre.h"
#include "asterfort/dylach.h"
#include "asterfort/dylech.h"
#include "asterfort/dylema.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mdallo.h"
#include "asterfort/mdarch.h"
#include "asterfort/mtcmbl.h"
#include "asterfort/mtdefs.h"
#include "asterfort/mtdscr.h"
#include "asterfort/omega2.h"
#include "asterfort/preres.h"
#include "asterfort/resoud.h"
#include "asterfort/resu60.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsagsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rsorac.h"
#include "asterfort/sigusr.h"
#include "asterfort/titre.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mesr.h"
#include "asterfort/u2mess.h"
#include "asterfort/utcrre.h"
#include "asterfort/utexcm.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
#include "asterfort/vtcrem.h"
#include "asterfort/wkvect.h"
#include "asterfort/zinit.h"
#include "blas/zcopy.h"
    integer :: ibid, nbold, isto1
    real(kind=8) :: r8bid
    complex(kind=8) :: c16bid
    character(len=8) :: k8bid
    character(len=19) :: k19bid
    character(len=8) :: baseno, resuco, result, resu1
    character(len=19) :: cn2mbr, vediri, veneum, vevoch, vassec
    character(len=19) :: lischa
    integer :: nbsym, i, n1, n2
    integer :: lamor1, lamor, limpe, lfreq, nbfreq
    integer :: neq, nbmat
    integer :: ie, jrefa
    integer :: ifreq, ieq, inom, ier
    integer :: lsecmb, jsecmb, jsolut, jvezer
    integer :: icoef, icode
    integer :: lvale, linst, iret, ladpa, jord, lmasse
    integer :: ldgec, lvgec, lagec, jordr, jfreq
    integer :: jdepl, jvite, jacce
    integer :: lmat(4), nbord, icomb
    integer :: jpomr, jrefe
    logical :: newcal, calgen
    real(kind=8) :: depi, freq, omega
    real(kind=8) :: rval, coef(6), tps1(4), rtab(2)
    complex(kind=8) :: cval, czero
    character(len=1) :: typres, typcst(4)
    character(len=4) :: typcal, nomsym(4)
    character(len=8) :: nomo
    character(len=24) :: carele, mate
    character(len=14) :: numddl, numdl1, numdl2, numdl3
    character(len=16) :: typcon, nomcmd, tysd, k16bid
    character(len=19) :: lifreq, masse, raide, amor, dynam, impe, chamno
    character(len=19) :: solveu, maprec, secmbr, soluti, vezero, crgc
    character(len=19) :: nomt, nomi
    character(len=24) :: nomat(4), basemo, matric(3)
    character(len=24) :: exreco, exresu
    integer :: nbexre
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
    call titre()
!
! --- INITIALISATIONS DIVERSES
!
    depi = r8depi()
    typres = 'C'
    lamor1 = 0
    czero = dcmplx(0.d0,0.d0)
!
! --- NOM DES STRUCTURES
!
    baseno = '&&OP0060'
    maprec = '&&OP0060.MAPREC'
    soluti = '&&OP0060.SOLUTI'
    vezero = '&&OP0060.VEZERO'
    lischa = '&&OP0060.LISCHA'
    vediri = '&&VEDIRI'
    veneum = '&&VENEUM'
    vevoch = '&&VEVOCH'
    vassec = '&&VASSEC'
    crgc = '&&OP0060_GCPC'
!
! --- NOM UTILISATEUR DU CONCEPT RESULTAT CREE PAR LA COMMANDE
!
    call getres(result, typcon, nomcmd)
!
! --- ON VERIFIE SI LE CONCEPT EST REENTRANT
!
    newcal = .true.
    call gcucon(result, typcon, iret)
    if (iret .gt. 0) then
        call getvid(' ', 'RESULTAT', scal=resuco, nbret=ibid)
        if (ibid .eq. 0) then
            newcal = .true.
        else
            call gettco(resuco, tysd)
            if (tysd .eq. typcon) then
                newcal = .false.
                if (result .ne. resuco) then
                    call u2mess('F', 'ALGORITH9_28')
                endif
            else
                call u2mess('F', 'ALGORITH9_29')
            endif
        endif
    endif
!
! --- CALGEN : FLAG POUR LES CALCULS SUR BASE GENERALISEE
    calgen=.false.
    if (typcon(1:9) .eq. 'HARM_GENE') then
        calgen=.true.
        typcal='HARM'
        isto1=0
!       --- CAS DE REPRISE DE CALCUL
        if (.not.newcal) then
            resu1 = result
            result='&&OP0060'
        endif
    endif
!
! --- LISTE DES FREQUENCES POUR LE CALCUL
!
    call getvid(' ', 'LIST_FREQ', scal=lifreq, nbret=n1)
    if (n1 .gt. 0) then
        call jeveuo(lifreq//'.VALE', 'L', lfreq)
        call jelira(lifreq//'.VALE', 'LONMAX', nbfreq)
    else
        call getvr8(' ', 'FREQ', nbval=0, nbret=nbfreq)
        nbfreq = - nbfreq
        call wkvect(baseno//'.LISTE.FREQ', 'V V R', nbfreq, lfreq)
        call getvr8(' ', 'FREQ', nbval=nbfreq, vect=zr(lfreq))
    endif
!
! --- NOM DES CHAMPS CALCULES
!
    call getvtx(' ', 'NOM_CHAM', nbval=3, vect=nomsym, nbret=nbsym)
    ASSERT(nbsym.le.3)
    if (typcon .eq. 'ACOU_HARMO') then
        nbsym = 1
        nomsym(1) = 'PRES'
    else
        call getvtx(' ', 'NOM_CHAM', nbval=3, vect=nomsym, nbret=nbsym)
        if (nbsym .eq. 0) then
            nbsym = 3
            nomsym(1) = 'DEPL'
            nomsym(2) = 'VITE'
            nomsym(3) = 'ACCE'
        endif
    endif
!
! --- RECUPERATION DES DESCRIPTEURS DES MATRICES ET DES MATRICES
!
    raide = ' '
    masse = ' '
    amor = ' '
    call dylema(baseno, nbmat, nomat, raide, masse,&
                amor, impe)
    ASSERT(nbmat.le.4)
    call getvid(' ', 'MATR_AMOR', scal=k19bid, nbret=lamor)
    call getvid(' ', 'MATR_IMPE_PHI', scal=k19bid, nbret=limpe)
    call getvr8('AMOR_MODAL', 'AMOR_REDUIT', iocc=1, nbval=0, nbret=n1)
    call getvid('AMOR_MODAL', 'LIST_AMOR', iocc=1, nbval=0, nbret=n2)
    if (n1 .ne. 0 .or. n2 .ne. 0) lamor1 = 1
!
! --- TEST: LES MATRICES SONT TOUTES BASEES SUR LA MEME NUMEROTATION ?
!
    numdl1 = ' '
    numdl2 = ' '
    numdl3 = ' '
    call dismoi('F', 'NOM_NUME_DDL', raide, 'MATR_ASSE', ibid,&
                numdl1, ie)
    call dismoi('F', 'NOM_NUME_DDL', masse, 'MATR_ASSE', ibid,&
                numdl2, ie)
    if (lamor .ne. 0) then
        call dismoi('F', 'NOM_NUME_DDL', amor, 'MATR_ASSE', ibid,&
                    numdl3, ie)
    else
        numdl3 = numdl2
    endif
!
    if ((numdl1.ne.numdl2) .or. (numdl1.ne.numdl3) .or. (numdl2.ne.numdl3)) then
        call u2mess('F', 'ALGORITH9_34')
    else
        numddl = numdl2
    endif
!
! --- LECTURE INFORMATIONS MECANIQUES
!
    call dydome(nomo, mate, carele)
!
! --- LECTURE DU CHARGEMENT
!
    call dylech(nomo, lischa, nbexre, exreco, exresu)
!
! --- CALCUL ET PRE-ASSEMBLAGE DU CHARGEMENT
!
    call dylach(nomo, mate, carele, lischa, numddl,&
                vediri, veneum, vevoch, vassec)
!
!============================================
! 3. ==> ALLOCATION DES RESULTATS
!============================================
!
    if (calgen) then
!     --- SI LE CALCUL EST SUR BASE GENERALISEE (NOUVEAU/REPRISE)
!       - RECUPERER LA BASE MODALE DE PROJECTION
        call jeveuo(masse(1:19)//'.REFA', 'L', lmasse)
        basemo = zk24(lmasse)
!
        call jeveuo(nomat(1), 'L', ibid)
        neq = zi(ibid+2)
!
!       - ALLOUER LES VECTEURS DE TRAVAIL
        call wkvect('&&OP0060.DEPGEC', 'G V C', neq, ldgec)
        call wkvect('&&OP0060.VITGEC', 'G V C', neq, lvgec)
        call wkvect('&&OP0060.ACCGEC', 'G V C', neq, lagec)
!       - ALLOUER LES VECTEURS DE STOCKAGE DES RESULTATS
!       - ON RECHERCHE LES CHAMPS A REMPLIR POUR LE CAS HARMONIQUE
        if (nbsym .eq. 0) then
            nbsym = 3
            nomsym(1) = 'DEPL'
            nomsym(2) = 'VITE'
            nomsym(3) = 'ACCE'
        endif
        call mdallo(result, basemo, masse, raide, amor,&
                    neq, r8bid, nbfreq, 0, k8bid,&
                    k8bid, 0, k8bid, 0, k8bid,&
                    jdepl, jvite, jacce, ibid, jordr,&
                    jfreq, ibid, ibid, ibid, ibid,&
                    ibid, ibid, ibid, ibid, k16bid,&
                    nbsym, nomsym, typcal, 'GLOB')
!
    else if (newcal) then
!     --- SI NOUVEAU CALCUL SUR BASE PHYSIQUE
        call utcrre(result, nbfreq)
        nbold=0
!
    else
!     --- SI REPRISE DE CALCUL SUR BASE PHYSIQUE
!       - AGRANDIR LA SD_RESULTAT DE NBOLD A NBOLD+NBFREQ
        call rsorac(result, 'LONUTI', ibid, r8bid, k8bid,&
                    c16bid, r8bid, 'ABSOLU', nbold, 1,&
                    ibid)
        call rsagsd(result, nbfreq+nbold)
    endif
!
    if (.not.calgen) then
!       --- SAUVEGARDE DE LA COLLECTION .REFD POUR LES CALCULS SUR BASE PHYS
        matric(1) = raide
        matric(2) = masse
        matric(3) = amor
        call refdaj('F', result, -1, numddl, 'DYNAMIQUE',&
                    matric, iret)
    endif
!
!
!
!============================================
! 4. ==> CALCUL DES TERMES DEPENDANT DE LA FREQUENCE ET RESOLUTION
!         DU SYSTEME FREQUENCE PAR FREQUENCE
!============================================
!
!====
! 4.1. ==> PREPARATION DU CALCUL ---
!====
!
    do 41 i = 1, nbmat
        call jeveuo(nomat(i), 'L', lmat(i))
41  continue
    neq = zi(lmat(1)+2)
    typcst(1) = 'R'
    typcst(2) = 'R'
    typcst(3) = 'C'
    typcst(4) = 'C'
    coef(1) = 1.d0
!
! --- CREATION DE LA MATRICE DYNAMIQUE
!
    dynam = baseno//'.DYNAMIC_MX'
!
    jpomr=0
    do 15 icomb = 1, nbmat
!        ON RECHERCHE UNE EVENTUELLE MATRICE NON SYMETRIQUE
        nomi =nomat(icomb)(1:19)
        call jeveuo(nomi//'.REFA', 'L', jrefe)
        if (zk24(jrefe-1+9) .eq. 'MR') then
            jpomr=icomb
        endif
15  continue
    if (jpomr .eq. 0) then
        if (lamor .ne. 0) then
            call mtdefs(dynam, amor, 'V', typres)
        else
            call mtdefs(dynam, raide, 'V', typres)
        endif
    else
        nomt = nomat(jpomr)(1:19)
        call mtdefs(dynam, nomt, 'V', typres)
    endif
    call mtdscr(dynam)
!
! --- CREATION DU VECTEUR SECOND-MEMBRE
!
    cn2mbr = '&&OP0060.SECOND.MBR'
    call wkvect(cn2mbr, 'V V C', neq, lsecmb)
!
! --- CREATION SD TEMPORAIRES
!
    secmbr = '&&OP0060.SECMBR'
    call vtcrem(secmbr, dynam, 'V', typres)
    call copisd('CHAMP_GD', 'V', secmbr, vezero)
    call jeveuo(secmbr(1:19)//'.VALE', 'E', jsecmb)
    call jeveuo(vezero(1:19)//'.VALE', 'E', jvezer)
    call zinit(neq, czero, zc(jvezer), 1)
!
! --- INFORMATIONS SOLVEUR
    solveu = '&&OP0060.SOLVEUR'
    call cresol(solveu)
!
!====
! 4.2 ==> BOUCLE SUR LES FREQUENCES ---
!====
    call uttcpu('CPU.OP0060', 'INIT', ' ')
!
    do 42 ifreq = 1, nbfreq
        call uttcpu('CPU.OP0060', 'DEBUT', ' ')
!
! ----- CALCUL DES COEFF. POUR LES MATRICES
!
        freq = zr(lfreq-1+ifreq)
        omega = depi*freq
        coef(2) = - omega2(freq)
        icoef = 2
        if ((lamor.ne.0) .or. (lamor1.ne.0)) then
            coef(3) = 0.d0
            coef(4) = omega
            icoef = 4
        endif
        if (limpe .ne. 0) then
            coef(icoef+1) = 0.d0
            coef(icoef+2) = coef(2) * depi * freq
        endif
!
! ----- CALCUL DU SECOND MEMBRE
!
        call dy2mbr(numddl, neq, lischa, freq, vediri,&
                    veneum, vevoch, vassec, lsecmb)
!
! ----- APPLICATION EVENTUELLE EXCIT_RESU
!
        if (nbexre .ne. 0) then
            call dyexre(numddl, freq, nbexre, exreco, exresu,&
                        lsecmb)
        endif
!
! ----- CALCUL DE LA MATRICE DYNAMIQUE
!
        call mtcmbl(nbmat, typcst, coef, nomat, dynam,&
                    ' ', ' ', 'ELIM=')
        call jeveuo(dynam(1:19)//'.REFA', 'E', jrefa)
        zk24(jrefa-1+7) = solveu
        zk24(jrefa-1+8) = ' '
!
! ----- FACTORISATION DE LA MATRICE DYNAMIQUE
!
        call preres(solveu, 'V', icode, maprec, dynam,&
                    ibid, -9999)
        if ((icode.eq.1) .or. (icode.eq.2)) then
            call u2mesr('I', 'DYNAMIQUE_14', 1, freq)
        endif
!
! ----- RESOLUTION DU SYSTEME, CELUI DU CHARGEMENT STANDARD
!
        call zcopy(neq, zc(lsecmb), 1, zc(jsecmb), 1)
        call resoud(dynam, maprec, solveu, vezero, 0,&
                    secmbr, soluti, 'V', r8bid, c16bid,&
                    crgc, .true., 0, iret)
        call jeveuo(soluti(1:19)//'.VALE', 'L', jsolut)
        call zcopy(neq, zc(jsolut), 1, zc(lsecmb), 1)
        call jedetr(soluti)
!
! ----------------------------------------------------------------
! --- ARCHIVAGE DES RESULTATS SUR BASE PHYSIQUE OU GENERALISEE ---
! ----------------------------------------------------------------
!
        if (.not.calgen) then
!       --- SI CALCUL SUR BASE PHYSIQUE
!         - CREER UN CHAM_NO DANS LA SD_RESULTAT
            do 130 inom = 1, nbsym
!         --- BOUCLE SUR LES CHAMPS A STOCKER (DEPL,VITE,ACCE)
                call rsexch(' ', result, nomsym(inom), ifreq+nbold, chamno,&
                            ier)
!
!           --- RECHERCHE SI IL EST "POSSIBLE" D'ECRIRE LE CHAMP DANS
!             - RESULTAT
                if (ier .eq. 0) then
!           --- LE CHAMPS EXISTE DEJA ALORS IL Y A UN PBLM, MESSAGE
!             - D'ALARME
                    call u2mesk('A', 'ALGORITH2_64', 1, chamno)
!
                else if (ier .eq. 100) then
!           --- LE CHAMPS N'EXISTE PAS ET IL EST POSSIBLE DE LE CREER
                    call vtcrem(chamno, masse, 'G', typres)
!             --- CREATION D'UN CHAM_NO S'APPUYANT SUR LA NUMEROTATION
!               - DE LA MATRICE ASSEMBLEE DE MASSE
!
                else
!           --- SI IL N'EST PAS POSSIBLE DE CREER LE CHAMP, ERR. FATALE
                    call u2mess('F', 'ALGORITH2_65')
                endif
!
!           --- RECOPIE DANS L'OBJET RESULTAT
                call jeveuo(chamno//'.VALE', 'E', lvale)
                if ((nomsym(inom) .eq. 'DEPL' ) .or. ( nomsym(inom) .eq. 'PRES' )) then
                    do 131 ieq = 0, neq-1
                        zc(lvale+ieq) = zc(lsecmb+ieq)
131                  continue
                else if (nomsym(inom) .eq. 'VITE') then
                    cval = dcmplx(0.d0,depi*freq)
                    do 132 ieq = 0, neq-1
                        zc(lvale+ieq) = cval * zc(lsecmb+ieq)
132                  continue
                else if (nomsym(inom) .eq. 'ACCE') then
                    rval = coef(2)
                    do 133 ieq = 0, neq-1
                        zc(lvale+ieq) = rval * zc(lsecmb+ieq)
133                  continue
                endif
                call rsnoch(result, nomsym(inom), ifreq+nbold)
                call jelibe(chamno//'.VALE')
130          continue
!         --- FIN DE LA BOUCLE 130 SUR LES CHAMPS A STOCKER
!
!         --- RECOPIE DE LA FREQUENCE DE STOCKAGE
            call rsadpa(result, 'E', 1, 'FREQ', ifreq+nbold,&
                        0, linst, k8bid)
            zr(linst) = freq
!
        else
!       --- SI CALCUL SUR BASE GENERALISEE
!         - REMPLISSAGE DES VECTEURS DE TRAVAIL: DEPGEC,VITGEC,ACCGEC
            do 140 inom = 1, nbsym
                if (nomsym(inom) .eq. 'DEPL') then
                    do 141 ieq = 0, neq-1
                        zc(ldgec+ieq) = zc(lsecmb+ieq)
141                  continue
                else if (nomsym(inom) .eq. 'VITE') then
                    cval = dcmplx(0.d0,depi*freq)
                    do 142 ieq = 0, neq-1
                        zc(lvgec+ieq) = cval * zc(lsecmb+ieq)
142                  continue
                else if (nomsym(inom) .eq. 'ACCE') then
                    rval = coef(2)
                    do 143 ieq = 0, neq-1
                        zc(lagec+ieq) = rval * zc(lsecmb+ieq)
143                  continue
                endif
!
140          continue
            call mdarch(isto1, ifreq-1, freq, r8bid, neq,&
                        typcal, nbsym, nomsym, r8bid, r8bid,&
                        r8bid, r8bid, r8bid, r8bid, zc(ldgec),&
                        zc( lvgec), zc(lagec), zc(jdepl), zc(jvite), zc(jacce),&
                        r8bid, zi( jordr), zr(jfreq))
            isto1=isto1+1
        endif
!
!
! ----- VERIFICATION SI INTERRUPTION DEMANDEE PAR SIGNAL USR1
!
        if (etausr() .eq. 1) then
            call sigusr()
        endif
!
! ----- MESURE CPU
!
        call uttcpu('CPU.OP0060', 'FIN', ' ')
        call uttcpr('CPU.OP0060', 4, tps1)
        if (tps1(4) .gt. .90d0*tps1(1) .and. i .ne. nbfreq) then
            rtab(1) = tps1(4)
            rtab(2) = tps1(1)
            call utexcm(28, 'DYNAMIQUE_13', 0, k8bid, 1,&
                        ifreq, 2, rtab)
        endif
42  continue
!
!     --- DETRUIRE LES OBJETS TEMPORAIRES A LA FIN DU CALCUL GENE
    if (calgen) then
        call jedetr('&&OP0060.DEPGEC')
        call jedetr('&&OP0060.VITGEC')
        call jedetr('&&OP0060.ACCGEC')
    endif
!
! --- STOCKAGE : MODELE,CARA_ELEM,CHAM_MATER, CALCUL PHYSIQUE
!
    if (.not.calgen) then
        call dismoi('F', 'NOM_MODELE', raide, 'MATR_ASSE', ibid,&
                    nomo, iret)
        call dismoi('F', 'CHAM_MATER', raide, 'MATR_ASSE', ibid,&
                    mate, iret)
        call dismoi('F', 'CARA_ELEM', raide, 'MATR_ASSE', ibid,&
                    carele, iret)
        call jeveuo(result//'           .ORDR', 'L', jord)
        call jelira(result//'           .ORDR', 'LONUTI', nbord)
        do 43 i = 1, nbord
            call rsadpa(result, 'E', 1, 'MODELE', zi(jord+i-1),&
                        0, ladpa, k8bid)
            zk8(ladpa) = nomo
            call rsadpa(result, 'E', 1, 'CHAMPMAT', zi(jord+i-1),&
                        0, ladpa, k8bid)
            zk8(ladpa) = mate(1:8)
            call rsadpa(result, 'E', 1, 'CARAELEM', zi(jord+i-1),&
                        0, ladpa, k8bid)
            zk8(ladpa) = carele(1:8)
43      continue
    endif
!
! --- CAS DE REPRISE AVEC CALCUL SUR BASE GENERALISE
!
    if (calgen .and. (.not.newcal)) then
        call resu60(resu1, result)
    endif
!
    call jedema()
end subroutine
