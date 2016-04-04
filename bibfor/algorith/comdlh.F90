subroutine comdlh()
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!  COMMANDE DYNA_VIBRA // OPTION TYPE_CALCUL = HARMONIQUE
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
#include "asterc/r8prem.h"
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
#include "asterfort/extdia.h"
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
#include "asterfort/mgutdm.h"
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
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"

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
    integer :: neq, nbmat, ifm, niv
    integer :: ifreq, ieq, inom, ier
    integer :: lsecmb, jvezer, nbmodi, nbmody, nbbas, j
    integer :: icoef, icode, nbmode, jrefe
    integer :: linst, iret, ladpa, dec
    integer :: ldgec, lvgec, lagec, jordr, jfreq
    integer :: jdepl, jvite, jacce
    integer :: lmat(4), nbord, icomb, sstruct, nbsst
    integer :: jpomr, freqpr, last_prperc, perc, nbpheq
    aster_logical :: newcal, calgen
    real(kind=8) :: depi, freq, omega, omeg2, fmin, fmax
    real(kind=8) :: rval, coef(6), tps1(4), rtab(2)
    real(kind=8) :: fcal_min, fcal_max, epsi
    complex(kind=8) :: cval, czero
    character(len=1) :: typres, typcst(4)
    character(len=4) :: typcal, nomsym(4)
    character(len=8) :: nomo, matass, modgen
    character(len=24) :: carele, mate
    character(len=14) :: numddl, numdl1, numdl2, numdl3, nddlphys
    character(len=16) :: typcon, nomcmd, tysd, champs
    character(len=19) :: lifreq, masse, raide, amor, dynam, impe, chamno
    character(len=19) :: solveu, maprec, secmbr, soluti, vezero, crgc
    character(len=19) :: nomt, nomi, print_type
    character(len=24) :: nomat(4), basemo, nume24, typco
    character(len=24) :: exreco, exresu
    integer :: nbexre, tmod(1)
    integer, pointer :: ordr(:) => null()
    character(len=24), pointer :: refa(:) => null()
    character(len=24), pointer :: refe(:) => null()
    character(len=24), pointer :: nlmasse(:) => null()
    complex(kind=8), pointer :: secmb(:) => null()
    complex(kind=8), pointer :: solut(:) => null()
    complex(kind=8), pointer :: nlvale(:) => null()
    integer, pointer :: nequ(:) => null()
    real(kind=8), pointer :: mass_dia(:) => null()
    real(kind=8), pointer :: rigi_dia(:) => null()
    real(kind=8), pointer :: puls(:)     => null()

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
    epsi = r8prem()
    typres = 'C'
    lamor1 = 0
    czero = dcmplx(0.d0,0.d0)
!
! --- NOM DES STRUCTURES
!
    baseno = '&&COMDLH'
    maprec = '&&COMDLH.MAPREC'
    soluti = '&&COMDLH.SOLUTI'
    vezero = '&&COMDLH.VEZERO'
    lischa = '&&COMDLH.LISCHA'
    vediri = '&&VEDIRI'
    veneum = '&&VENEUM'
    vevoch = '&&VEVOCH'
    vassec = '&&VASSEC'
    crgc = '&&COMDLH_GCPC'
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
            result='&&COMDLH'
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
        call wkvect('&&COMDLH.DEPGEC', 'G V C', neq, ldgec)
        call wkvect('&&COMDLH.VITGEC', 'G V C', neq, lvgec)
        call wkvect('&&COMDLH.ACCGEC', 'G V C', neq, lagec)
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
    cn2mbr = '&&COMDLH.SECOND.MBR'
    call wkvect(cn2mbr, 'V V C', neq, lsecmb)
!
! --- CREATION SD TEMPORAIRES
!
    secmbr = '&&COMDLH.SECMBR'
    call vtcrem(secmbr, dynam, 'V', typres)
    call copisd('CHAMP_GD', 'V', secmbr, vezero)
    call jeveuo(secmbr(1:19)//'.VALE', 'E', vc=secmb)
    call jeveuo(vezero(1:19)//'.VALE', 'E', jvezer)
    call vecinc(neq, czero, zc(jvezer))
!
! --- INFORMATIONS SOLVEUR
    solveu = '&&COMDLH.SOLVEUR'
    call cresol(solveu)

!
! --- EXTRA INFORMATION FOR REDUCED MODAL CALCULATIONS

    if (calgen) then
        call gettco(basemo,typco)
        sstruct = 0
        if (typco(1:9) .eq. 'MODE_MECA') then
            call dismoi('NUME_DDL', basemo, 'RESU_DYNA', repk=nddlphys)
            call dismoi('NB_EQUA', nddlphys, 'NUME_DDL', repi=nbpheq)
        else if (typco(1:9).eq.'MODE_GENE') then
            call dismoi('REF_RIGI_PREM', basemo, 'RESU_DYNA', repk=matass)
            call dismoi('NOM_NUME_DDL', matass, 'MATR_ASSE', repk=nddlphys)
            call jeveuo(nddlphys(1:14)//'.NUME.NEQU', 'L', vi=nequ)
            nbpheq = nequ(1)
        else
            sstruct = 1
        endif

        nbmode = neq
        nume24 = numddl
        AS_ALLOCATE(vr=mass_dia, size=nbmode)
        AS_ALLOCATE(vr=rigi_dia, size=nbmode)
        AS_ALLOCATE(vr=puls, size=nbmode)
        call extdia(masse, nume24, sstruct, mass_dia)
        call extdia(raide, nume24, sstruct, rigi_dia)

        if (sstruct.eq.1) then
            call jeveuo(numddl(1:14)//'.NUME.REFN', 'L', jrefe)
            modgen = zk24(jrefe)(1:8)
            call jelira(modgen//'      .MODG.SSNO', 'NOMMAX', nbsst)

            nbmodi = 0
            nbmody = 0
            do i = 1, nbsst
                call mgutdm(modgen, ' ', i, 'NOM_BASE_MODALE', ibid, basemo)
                call dismoi('NB_MODES_DYN', basemo, 'RESULTAT', repi=nbbas)
                do j = 1, nbbas
                    omeg2 = 0.d0
                    if (mass_dia(nbmodi+j).gt.epsi) then
                        omeg2 = abs(rigi_dia(nbmodi+j)/mass_dia(nbmodi+j))
                    end if
                    puls (nbmodi+j) = sqrt(omeg2)
                end do
                nbmody = nbmody + nbbas
                call dismoi('NB_MODES_TOT', basemo, 'RESULTAT', repi=nbbas)
                nbmodi = nbmodi + nbbas
            end do
        else
            do i = 1, nbmode
                omeg2 = 0.d0
                if (mass_dia(i).gt.epsi) then
                    omeg2 = abs(rigi_dia(i)/mass_dia(i))
                end if
                puls (i) = sqrt(omeg2)
            enddo
        end if

        fmin =  1.d25
        fmax = -1.d25
        do i = 1, nbmode
            freq = puls(i)/depi
            if (freq.lt.fmin) fmin = freq
            if (freq.gt.fmax) fmax = freq
        enddo

        AS_DEALLOCATE(vr=mass_dia)
        AS_DEALLOCATE(vr=rigi_dia)
        AS_DEALLOCATE(vr=puls)
    else
        call dismoi('NOM_MODELE', raide, 'MATR_ASSE', repk=nomo)
    end if

    fcal_min =  1.d25
    fcal_max = -1.d25
    do i = 1, nbfreq
        freq = zr(lfreq-1+i)
        if (freq.lt.fcal_min) fcal_min = freq
        if (freq.gt.fcal_max) fcal_max = freq
    enddo
!
! --- IMPRESSIONS RECAPITULATIVES POUR L'UTILISATEUR
    print_type             = 'PHYSique'
    if (calgen) print_type = 'GENEralisee'
    call utmess('I', 'DYNAMIQUE_55', nk=3, valk=['D Y N A _ V I B R A',&
                                                 'HARMonique         ',&
                                                 print_type])

    if (calgen) then
!       1 - Calculation type : standard, substructuring
        if (sstruct.eq.0) then
            call utmess('I', 'DYNAMIQUE_56', nk=1, valk=[basemo],&
                                             ni=1, vali=[nbpheq])
        else
            call utmess('I', 'DYNAMIQUE_57', nk=2, valk=[basemo, numddl])
        end if
!       2 - Minimum and max frequencies
        call utmess('I', 'DYNAMIQUE_59', ni=1, vali=[nbmode],&
                                         nr=2, valr=[fmin, fmax])
    else
        call utmess('I', 'DYNAMIQUE_82', sk=nomo, si=neq)
    end if

!   3 - Dynamic matrices
    call utmess('I', 'DYNAMIQUE_60')
    call utmess('I', 'DYNAMIQUE_61', nk=2, valk=[masse, raide])
    if (lamor.ne.0) then
        call utmess('I', 'DYNAMIQUE_62', sk=amor)
    else if (lamor1.ne.0) then
        call utmess('I', 'DYNAMIQUE_63')
    else
        call utmess('I', 'DYNAMIQUE_64')
    end if
    if (limpe.ne.0) call utmess('I', 'DYNAMIQUE_87', sk=impe)

!   4 - Calculation and saving parameters
    champs = ' '
    do i = 1, nbsym
        dec = 5*(i-1)
        champs(dec+i:dec+i+3) = nomsym(i)(1:4)
    end do
    call utmess('I', 'DYNAMIQUE_88', nr=2, valr= [fcal_min, fcal_max],&
                                     si=nbfreq, sk=champs)

!
!====
! 4.2 ==> BOUCLE SUR LES FREQUENCES ---
!====
    call uttcpu('CPU.COMDLH', 'INIT', ' ')

!   NIVEAU D'IMPRESSION DE L'AVANCEMENT DE CALCUL
    freqpr = 5
    if (niv.eq.2) freqpr = 1
    last_prperc = 999

    do ifreq = 1, nbfreq
        call uttcpu('CPU.COMDLH', 'DEBUT', ' ')
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
        perc = int(100.d0*(real(ifreq)/real(nbfreq)))
        if ((perc.ne.last_prperc).or.(ifreq.eq.1) )then
            if ((mod(perc,freqpr).eq.0).or.(ifreq.eq.1)) then
                call utmess('I', 'DYNAMIQUE_95', ni=2, vali=[perc, ifreq],&
                                                 sr=freq)
                last_prperc = perc
            end if
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
        call uttcpu('CPU.COMDLH', 'FIN', ' ')
        call uttcpr('CPU.COMDLH', 4, tps1)
        if (tps1(4) .gt. .90d0*tps1(1) .and. i .ne. nbfreq) then
            rtab(1) = tps1(4)
            rtab(2) = tps1(1)
            call utmess('Z', 'DYNAMIQUE_13', si=ifreq, nr=2, valr=rtab,&
                        num_except=28)
        endif
    end do

!
!     --- DETRUIRE LES OBJETS TEMPORAIRES A LA FIN DU CALCUL GENE
    if (calgen) then
        call jedetr('&&COMDLH.DEPGEC')
        call jedetr('&&COMDLH.VITGEC')
        call jedetr('&&COMDLH.ACCGEC')
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
