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
#include "asterf_types.h"
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
#include "asterfort/infniv.h"
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
#include "asterfort/refdaj.h"
#include "asterfort/resoud.h"
#include "asterfort/resu60.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsagsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rsorac.h"
#include "asterfort/sigusr.h"
#include "asterfort/titre.h"
#include "asterfort/utcrre.h"
#include "asterfort/utmess.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
#include "asterfort/vecinc.h"
#include "asterfort/vtcrem.h"
#include "asterfort/wkvect.h"
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
    integer :: neq, nbmat, ifm, niv, pasfreq
    integer :: ifreq, ieq, inom, ier
    integer :: lsecmb, jvezer
    integer :: icoef, icode
    integer :: linst, iret, ladpa
    integer :: ldgec, lvgec, lagec, jordr, jfreq
    integer :: jdepl, jvite, jacce
    integer :: lmat(4), nbord, icomb
    integer :: jpomr
    aster_logical :: newcal, calgen
    real(kind=8) :: depi, freq, omega
    real(kind=8) :: rval, coef(6), tps1(4), rtab(2)
    complex(kind=8) :: cval, czero
    character(len=1) :: typres, typcst(4)
    character(len=4) :: typcal, nomsym(4)
    character(len=8) :: nomo
    character(len=24) :: carele, mate
    character(len=14) :: numddl, numdl1, numdl2, numdl3
    character(len=16) :: typcon, nomcmd, tysd
    character(len=19) :: lifreq, masse, raide, amor, dynam, impe, chamno
    character(len=19) :: solveu, maprec, secmbr, soluti, vezero, crgc
    character(len=19) :: nomt, nomi
    character(len=24) :: nomat(4), basemo
    character(len=24) :: exreco, exresu
    integer :: nbexre, tmod(1)
    integer, pointer :: ordr(:) => null()
    character(len=24), pointer :: refa(:) => null()
    character(len=24), pointer :: refe(:) => null()
    character(len=24), pointer :: nlmasse(:) => null()
    complex(kind=8), pointer :: secmb(:) => null()
    complex(kind=8), pointer :: solut(:) => null()
    complex(kind=8), pointer :: nlvale(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call titre()
!
    call infmaj()
    call infniv(ifm,niv)
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
                    call utmess('F', 'ALGORITH9_28')
                endif
            else
                call utmess('F', 'ALGORITH9_29')
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
    nomsym(1) = ' '
    nomsym(2) = ' '
    nomsym(3) = ' '
    call getvtx(' ', 'NOM_CHAM', nbval=3, nbret=nbsym)
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
    call dismoi('NOM_NUME_DDL', raide, 'MATR_ASSE', repk=numdl1)
    call dismoi('NOM_NUME_DDL', masse, 'MATR_ASSE', repk=numdl2)
    if (lamor .ne. 0) then
        call dismoi('NOM_NUME_DDL', amor, 'MATR_ASSE', repk=numdl3)
    else
        numdl3 = numdl2
    endif
!
    if ((numdl1.ne.numdl2) .or. (numdl1.ne.numdl3) .or. (numdl2.ne.numdl3)) then
        call utmess('F', 'ALGORITH9_34')
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
        call jeveuo(masse(1:19)//'.REFA', 'L', vk24=nlmasse)
        basemo = nlmasse(1)
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
!
        call mdallo(result, 'HARM', nbfreq, sauve='GLOB', base=basemo,&
                    mass=masse, rigi=raide, amor=amor, nbmodes=neq, jordr=jordr,&
                    jdisc=jfreq, jdepl=jdepl, jvite=jvite, jacce=jacce, nbsym=nbsym,&
                    nomsym=nomsym)
!
!
    else if (newcal) then
!     --- SI NOUVEAU CALCUL SUR BASE PHYSIQUE
        call utcrre(result, nbfreq)
        nbold=0
!
    else
!     --- SI REPRISE DE CALCUL SUR BASE PHYSIQUE
!       - AGRANDIR LA SD_RESULTAT DE NBOLD A NBOLD+NBFREQ
        call rsorac(result, 'LONUTI', 0, r8bid, k8bid,&
                    c16bid, r8bid, 'ABSOLU', tmod, 1,&
                    ibid)
        nbold=tmod(1)
        call rsagsd(result, nbfreq+nbold)
    endif
!
    if (.not.calgen) then
!       --- SAUVEGARDE DE LA COLLECTION .REFD POUR LES CALCULS SUR BASE PHYS
        call refdaj('F', result, nbfreq, numddl, 'DYNAMIQUE',&
                    [raide, masse, amor], iret)
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
    do i = 1, nbmat
        call jeveuo(nomat(i), 'L', lmat(i))
    end do
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
    do icomb = 1, nbmat
!        ON RECHERCHE UNE EVENTUELLE MATRICE NON SYMETRIQUE
        nomi =nomat(icomb)(1:19)
        call jeveuo(nomi//'.REFA', 'L', vk24=refe)
        if (refe(9) .eq. 'MR') then
            jpomr=icomb
        endif
    end do
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
    call jeveuo(secmbr(1:19)//'.VALE', 'E', vc=secmb)
    call jeveuo(vezero(1:19)//'.VALE', 'E', jvezer)
    call vecinc(neq, czero, zc(jvezer))
!
! --- INFORMATIONS SOLVEUR
    solveu = '&&OP0060.SOLVEUR'
    call cresol(solveu)
!
!====
! 4.2 ==> BOUCLE SUR LES FREQUENCES ---
!====
    call uttcpu('CPU.OP0060', 'INIT', ' ')

!   NIVEAU D'IMPRESSION DE L'AVANCEMENT DE CALCUL
    pasfreq = 1
    if (niv.eq.1) then
        pasfreq = max(1,nint(1.d0*nbfreq/20.d0))
    end if
    if (pasfreq.ne.1) then
        call utmess('I', 'DYNAMIQUE_28', nk=3, valk=nomsym, si=1+nbold, &
                    sr=zr(lfreq))
    end if
!
    do ifreq = 1, nbfreq
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
        call jeveuo(dynam(1:19)//'.REFA', 'E', vk24=refa)
        refa(7) = solveu
        refa(8) = ' '
!
! ----- FACTORISATION DE LA MATRICE DYNAMIQUE
!
        call preres(solveu, 'V', icode, maprec, dynam,&
                    ibid, -9999)
        if ((icode.eq.1) .or. (icode.eq.2)) then
            call utmess('I', 'DYNAMIQUE_14', sr=freq)
        endif
!
! ----- RESOLUTION DU SYSTEME, CELUI DU CHARGEMENT STANDARD
!
        call zcopy(neq, zc(lsecmb), 1, secmb, 1)
        call resoud(dynam, maprec, solveu, vezero, 0,&
                    secmbr, soluti, 'V', [0.d0], [c16bid],&
                    crgc, .true._1, 0, iret)
        call jeveuo(soluti(1:19)//'.VALE', 'L', vc=solut)
        call zcopy(neq, solut, 1, zc(lsecmb), 1)
        call jedetr(soluti)
!
! ----- IMPRESSION DE L'ETAT D'AVANCEMENT DU CALCUL FREQUENTIEL
!
        if (mod(ifreq, pasfreq) .eq. 0) then
            call utmess('I', 'DYNAMIQUE_28', nk=3, valk=nomsym, si=ifreq+nbold, &
                        sr=freq)
        end if
!
! ----------------------------------------------------------------
! --- ARCHIVAGE DES RESULTATS SUR BASE PHYSIQUE OU GENERALISEE ---
! ----------------------------------------------------------------
!

        if (.not.calgen) then       
!       --- SI CALCUL SUR BASE PHYSIQUE
!         - CREER UN CHAM_NO DANS LA SD_RESULTAT
            do inom = 1, nbsym
!         --- BOUCLE SUR LES CHAMPS A STOCKER (DEPL,VITE,ACCE)
                call rsexch(' ', result, nomsym(inom), ifreq+nbold, chamno,&
                            ier)
!
!           --- RECHERCHE SI IL EST "POSSIBLE" D'ECRIRE LE CHAMP DANS
!             - RESULTAT
                if (ier .eq. 0) then
!           --- LE CHAMPS EXISTE DEJA ALORS IL Y A UN PBLM, MESSAGE
!             - D'ALARME
                    call utmess('A', 'ALGORITH2_64', sk=chamno)
!
                else if (ier .eq. 100) then
!           --- LE CHAMPS N'EXISTE PAS ET IL EST POSSIBLE DE LE CREER
                    call vtcrem(chamno, masse, 'G', typres)
!             --- CREATION D'UN CHAM_NO S'APPUYANT SUR LA NUMEROTATION
!               - DE LA MATRICE ASSEMBLEE DE MASSE
!
                else
!           --- SI IL N'EST PAS POSSIBLE DE CREER LE CHAMP, ERR. FATALE
                    call utmess('F', 'ALGORITH2_65')
                endif
!
!           --- RECOPIE DANS L'OBJET RESULTAT
                call jeveuo(chamno//'.VALE', 'E', vc=nlvale)
                if ((nomsym(inom) .eq. 'DEPL' ) .or. ( nomsym(inom) .eq. 'PRES' )) then
                    do ieq = 0, neq-1
                        nlvale(ieq+1) = zc(lsecmb+ieq)
                    end do
                else if (nomsym(inom) .eq. 'VITE') then
                    cval = dcmplx(0.d0,depi*freq)
                    do ieq = 0, neq-1
                        nlvale(ieq+1) = cval * zc(lsecmb+ieq)
                    end do
                else if (nomsym(inom) .eq. 'ACCE') then
                    rval = coef(2)
                    do ieq = 0, neq-1
                        nlvale(ieq+1) = rval * zc(lsecmb+ieq)
                    end do
                endif
                call rsnoch(result, nomsym(inom), ifreq+nbold)
                call jelibe(chamno//'.VALE')
            end do
!         --- FIN DE LA BOUCLE 130 SUR LES CHAMPS A STOCKER
!
!         --- RECOPIE DE LA FREQUENCE DE STOCKAGE
            call rsadpa(result, 'E', 1, 'FREQ', ifreq+nbold,&
                        0, sjv=linst, styp=k8bid)
            zr(linst) = freq
!
        else
!       --- SI CALCUL SUR BASE GENERALISEE
!         - REMPLISSAGE DES VECTEURS DE TRAVAIL: DEPGEC,VITGEC,ACCGEC
            do inom = 1, nbsym
                if (nomsym(inom) .eq. 'DEPL') then
                    do ieq = 0, neq-1
                        zc(ldgec+ieq) = zc(lsecmb+ieq)
                    end do
                else if (nomsym(inom) .eq. 'VITE') then
                    cval = dcmplx(0.d0,depi*freq)
                    do ieq = 0, neq-1
                        zc(lvgec+ieq) = cval * zc(lsecmb+ieq)
                    end do
                else if (nomsym(inom) .eq. 'ACCE') then
                    rval = coef(2)
                    do ieq = 0, neq-1
                        zc(lagec+ieq) = rval * zc(lsecmb+ieq)
                    end do
                endif
!
            end do
            call mdarch('HARM', isto1, ifreq-1, freq, neq,&
                        zi(jordr), zr(jfreq), nbsym=nbsym, nomsym=nomsym, depgec=zc(ldgec),&
                        vitgec=zc(lvgec), accgec=zc(lagec), depstc=zc(jdepl), vitstc=zc(jvite),&
                        accstc=zc(jacce))
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
            call utmess('Z', 'DYNAMIQUE_13', si=ifreq, nr=2, valr=rtab,&
                        num_except=28)
        endif
    end do

!
! ----- IMPRESSION DE LA DERNIERE FREQUENCE CALCULEE
!
    ifreq = ifreq - 1
    if (mod((ifreq), pasfreq) .gt. 0) then
        call utmess('I', 'DYNAMIQUE_28', nk=3, valk=nomsym, si=ifreq+nbold, &
                    sr=freq)
    end if

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
        call dismoi('NOM_MODELE', raide, 'MATR_ASSE', repk=nomo)
        call dismoi('CHAM_MATER', raide, 'MATR_ASSE', repk=mate)
        call dismoi('CARA_ELEM', raide, 'MATR_ASSE', repk=carele)
        call jeveuo(result//'           .ORDR', 'L', vi=ordr)
        call jelira(result//'           .ORDR', 'LONUTI', nbord)
        do i = 1, nbord
            call rsadpa(result, 'E', 1, 'MODELE', ordr(i),&
                        0, sjv=ladpa, styp=k8bid)
            zk8(ladpa) = nomo
            call rsadpa(result, 'E', 1, 'CHAMPMAT', ordr(i),&
                        0, sjv=ladpa, styp=k8bid)
            zk8(ladpa) = mate(1:8)
            call rsadpa(result, 'E', 1, 'CARAELEM', ordr(i),&
                        0, sjv=ladpa, styp=k8bid)
            zk8(ladpa) = carele(1:8)
        end do
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
