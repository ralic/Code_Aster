subroutine harm75(nomres, typres, nomin, basemo)
    implicit none
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     OPERATEUR DE RETOUR A LA BASE PHYSIQUE A PARTIR DE DONNEES
!     GENERALISEES DANS LE CAS D'UN CALCUL HARMONIQUE
!     ------------------------------------------------------------------
! IN  : NOMRES : NOM UTILISATEUR POUR LA COMMANDE REST_BASE_PHYS
! IN  : TYPRES : TYPE DE RESULTAT : 'DYNA_HARMO'
! IN  : NOMIN  : NOM UTILISATEUR DU CONCEPT HARM_GENE AMONT
! IN  : NOMCMD : NOM DE LA COMMANDE : 'REST_BASE_PHYS'
! IN  : BASEMO : NOM UTILISATEUR DU CONCEPT MODE_MECA AMONT
!                (SI CALCUL MODAL PAR SOUS-STRUCTURATION)
!                ' ' SINON
! ----------------------------------------------------------------------
!
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cnocre.h"
#include "asterfort/copmod.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mdgepc.h"
#include "asterfort/rbph01.h"
#include "asterfort/rbph02.h"
#include "asterfort/refdcp.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rstran.h"
#include "asterfort/titre.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcreb.h"
#include "asterfort/vtcrec.h"
#include "asterfort/vtdefs.h"
#include "asterfort/wkvect.h"
#include "asterfort/nueq_chck.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
!
    integer :: ibid, nbmode, itresu(8)
    real(kind=8) :: epsi
    complex(kind=8) :: cbid
    character(len=1) :: type1
    character(len=8) :: k8b, basemo, crit, champ(8), interp, nomres, nomin, mode
    character(len=8) :: touch, mailla, matgen
    character(len=8) :: nomgd, basem2, blanc
    character(len=14) :: numddl
    character(len=16) :: typres, typbas(8), typcha, type(3)
    character(len=19) :: knume, kfreq, hrange, prchno, prof, typref(8)
    character(len=24) :: matric, chamno, crefe(2), chmod, nomcha, objve1, objve2
    character(len=24) :: objve3, objve4
    aster_logical :: tousno, leffor, prems
    integer :: inocmp, inoecp, inumno, inuddl
    integer :: j, jc, i, iarchi, ich
    integer :: idvecg, iret, iretou, jfreq
    integer :: jnume, lfreq, llcha, lvale, nbcham, nbinsg
    integer :: n1, n2, n3, n4, idec, idefm, idinsg, idresu
    integer :: nbfreq, neq, nbnoeu, ncmp
    real(kind=8), pointer :: base(:) => null()
    character(len=24), pointer :: refn(:) => null()
    integer, pointer :: desc(:) => null()
    cbid = dcmplx(0.d0, 0.d0)
! ------------------------------------------------------------------
    data chamno   /'&&HARM75.CHAMNO'/
    data blanc    /'        '/
! ------------------------------------------------------------------
!
    call jemarq()
!
    matric=' '
!
    nomcha=' '
    numddl=' '
    prchno=' '
!
    mode = basemo
    hrange = nomin
!
    call getvtx(' ', 'TOUT_CHAM', scal=touch, nbret=n1)
    if (n1 .ne. 0) then
        ASSERT(touch(1:3).eq.'OUI')
        nbcham = 3
        type(1) = 'DEPL            '
        type(2) = 'VITE            '
        type(3) = 'ACCE            '
    else
        call getvtx(' ', 'NOM_CHAM', nbval=0, nbret=n1)
        nbcham = -n1
        call getvtx(' ', 'NOM_CHAM', nbval=nbcham, vect=champ, nbret=n1)
        do i = 1, nbcham
            if (champ(i)(1:4) .eq. 'DEPL') then
                type(i) = 'DEPL            '
            else if (champ(i)(1:4).eq.'VITE') then
                type(i) = 'VITE            '
            else if (champ(i)(1:4).eq.'ACCE') then
                type(i) = 'ACCE            '
            else
!           CHAMP IGNORE
            endif
        end do
    endif
!
!
!     --- RECUPERATION DES ENTITES DU MAILLAGE SUR LESQUELLES ---
!     ---                PORTE LA RESTITUTION                 ---
    tousno = .true.
    call getvtx(' ', 'GROUP_NO', nbval=0, nbret=n1)
    call getvtx(' ', 'NOEUD', nbval=0, nbret=n2)
    call getvtx(' ', 'GROUP_MA', nbval=0, nbret=n3)
    call getvtx(' ', 'MAILLE', nbval=0, nbret=n4)
    if (n1+n2+n3+n4 .ne. 0) tousno = .false.
!
!     --- RECUPERATION DE LA BASE MODALE ---
! ON SUPPOSE QU ELLE EST ISSUE D UN MODE_MECA
!
    call jeveuo(hrange//'.DESC', 'L', vi=desc)
!
    nbmode = desc(2)
!
    if (mode .eq. ' ') then
!
        call dismoi('REF_RIGI_PREM', hrange, 'RESU_DYNA', repk=matgen, arret='C',&
                    ier=iret)
        call dismoi('BASE_MODALE', hrange, 'RESU_DYNA', repk=basemo)
!
        if (matgen(1:8) .ne. blanc) then
            call dismoi('REF_RIGI_PREM', basemo, 'RESU_DYNA', repk=matric)
            if (matric .ne. blanc) then
                call dismoi('NOM_NUME_DDL', matric, 'MATR_ASSE', repk=numddl)
            else
                call dismoi('NUME_DDL', basemo, 'RESU_DYNA', repk=numddl)
            endif
            prchno=numddl//'.NUME'
            call dismoi('NOM_GD', numddl, 'NUME_DDL', repk=nomgd)
            call dismoi('NOM_MAILLA', numddl, 'NUME_DDL', repk=mailla)
            if (tousno) call dismoi('NB_EQUA', numddl, 'NUME_DDL', repi=neq)
        else
!          -- POUR LES CALCULS SANS MATRICE GENERALISEE
!             (PROJ_MESU_MODAL)
            call dismoi('NUME_DDL', basemo, 'RESU_DYNA', repk=matric)
            if (matric(1:8) .eq. blanc) then
                call dismoi('REF_RIGI_PREM', basemo, 'RESU_DYNA', repk=matric)
                call dismoi('NOM_NUME_DDL', matric, 'MATR_ASSE', repk=numddl)
            else
                numddl = matric(1:8)
            endif
            prchno=numddl//'.NUME'
            call jeveuo(numddl//'.NUME.REFN', 'L', vk24=refn)
            matric = refn(1)
            mailla = matric(1:8)
            call dismoi('REF_RIGI_PREM', basemo, 'RESU_DYNA', repk=matric)
            if (tousno) call dismoi('NB_EQUA', numddl, 'NUME_DDL', repi=neq)
        endif
!
        basem2 = basemo
    else
!         --- BASE MODALE CALCULEE PAR SOUS-STRUCTURATION
!
        call rsexch('F', basemo, 'DEPL', 1, chmod,&
                    iret)
        chmod = chmod(1:19)//'.REFE'
        call dismoi('NOM_GD', chmod, 'CHAM_NO', repk=nomgd)
        call dismoi('PROF_CHNO', chmod, 'CHAM_NO', repk=prchno)
        call jeveuo(chmod, 'L', llcha)
        mailla = zk24(llcha)(1:8)
        crefe(1) = zk24(llcha)
        crefe(2) = zk24(llcha+1)
        if (tousno) then
            call nueq_chck(prchno, nb_equaz = neq)
        endif
        basem2 = ' '
    endif
!
!     ---   RECUPERATION DES VECTEURS DEPLACEMENT, VITESSE ET   ---
!     --- ACCELERATION GENERALISES SUIVANT LES CHAMPS SOUHAITES ---
    call rbph01(hrange, nbcham, type, itresu, 0,&
                basem2, typref, typbas, tousno, .false._1)
!
!     --- RECUPERATION DES NUMEROS DES NOEUDS ET DES DDLS ASSOCIES ---
!     ---         DANS LE CAS D'UNE RESTITUTION PARTIELLE          ---
!
    if (.not. tousno) then
        objve1 = '&&HARM75.NUME_NOEUD  '
        objve2 = '&&HARM75.NOM_CMP     '
        objve3 = '&&HARM75.NB_NEQ      '
        objve4 = '&&HARM75.NUME_DDL    '
        call rbph02(mailla, numddl, chmod, nomgd, neq,&
                    nbnoeu, objve1, ncmp, objve2, objve3,&
                    objve4)
        call jeveuo(objve1, 'L', inumno)
        call jeveuo(objve2, 'L', inocmp)
        call jeveuo(objve3, 'L', inoecp)
        call jeveuo(objve4, 'L', inuddl)
    endif
!
!
!     --- RECUPERATION DES FREQUENCES ---
    call getvtx(' ', 'CRITERE', scal=crit, nbret=n1)
    call getvr8(' ', 'PRECISION', scal=epsi, nbret=n1)
    call getvtx(' ', 'INTERPOL', scal=interp, nbret=n1)
! ON PLANTE LE CALCUL SI ON DEMANDE D'INTERPOLER EN FREQUENCIEL
    if (interp(1:3) .ne. 'NON') then
        call utmess('F', 'ALGORITH3_86')
    endif
!
    knume = '&&HARM75.NUM_RANG'
    kfreq = '&&HARM75.FREQ'
!
    call rstran(interp, hrange, ' ', 1, kfreq,&
                knume, nbfreq, iretou)
!
    if (iretou .ne. 0) then
        call utmess('F', 'UTILITAI4_24')
    endif
    call jeexin(kfreq, iret)
    if (iret .gt. 0) then
        call jeveuo(kfreq, 'L', jfreq)
        call jeveuo(knume, 'L', jnume)
    endif
!
!     --- CREATION DE LA SD RESULTAT ---
!
    call rscrsd('G', nomres, typres, nbfreq)
!
!     --- RESTITUTION SUR LA BASE REELLE ---
!
    call jeveuo(hrange//'.DISC', 'L', idinsg)
    call jelira(hrange//'.DISC', 'LONMAX', nbinsg)
    call wkvect('&&HARM75.VECTGENE', 'V V C', nbmode, idvecg)
    do ich = 1, nbcham
        leffor=.true.
        if (type(ich) .eq. 'DEPL' .or. type(ich) .eq. 'VITE' .or. type(ich) .eq. 'ACCE') &
        leffor=.false.
!
!            --- RECUPERATION DES DEFORMEES MODALES ---
!
        typcha = typbas(ich)
        call rsexch('F', basemo, typcha, 1, nomcha,&
                    iret)
        nomcha = nomcha(1:19)//'.VALE'
        call jeexin(nomcha, ibid)
        if (ibid .gt. 0) then
            nomcha(20:24)='.VALE'
        else
            nomcha(20:24)='.CELV'
        endif
!
        if (leffor) call jelira(nomcha, 'LONMAX', neq)
        AS_ALLOCATE(vr=base, size=nbmode*neq)
! CAS DE LA RESTITUTION SUR TOUTE LA STRUCTURE
        if (tousno) then
!           fournir nequa est indispensable parce que nous passons un prof_chno a copmod
            call copmod(basemo, bmodr=base, champ=typcha, numer=prchno(1:14), nequa=neq)
! CAS DE LA RESTITUTION SUR UNE PARTIE DE LA STRUCTURE SEULEMENT
        else
            do j = 1, nbmode
                call rsexch('F', basemo, typcha, j, nomcha,&
                            iret)
                call jeexin(nomcha(1:19)//'.VALE', ibid)
                if (ibid .gt. 0) then
                    nomcha(20:24)='.VALE'
                else
                    nomcha(20:24)='.CELV'
                endif
                call jeveuo(nomcha, 'L', idefm)
                idec = 0
                do i = 1, nbnoeu
                    do jc = 1, ncmp
                        if (zi(inoecp-1+(i-1)*ncmp+jc) .eq. 1) then
                            idec = idec + 1
                            base(1+(j-1)*neq+idec-1) = zr( idefm+zi( inuddl+idec-1)-1 )
                        endif
                    end do
                end do
            end do
        endif
! FIN DE LA RECUPERATION DE LA BASE MODALE
!
!  RESTITUTION PROPREMENT DITE
!
        iarchi = 0
        idresu = itresu(ich)
        prems=.true.
        do i = 0, nbfreq-1
            iarchi = iarchi + 1
            call rsexch(' ', nomres, type(ich), iarchi, chamno,&
                        iret)
            if (iret .eq. 0) then
                call utmess('A', 'ALGORITH2_64', sk=chamno)
            else if (iret .eq. 100) then
                if (tousno) then
                    if (mode .eq. blanc) then
                        if (leffor) then
                            call vtdefs(chamno, typref(ich), 'G', 'C')
                        else
                            call vtcreb(chamno, 'G', 'C',&
                                        nume_ddlz = numddl,&
                                        nb_equa_outz = neq)
                        endif
                    else
                        call vtcrec(chamno, chmod, 'G', 'C', neq)
                    endif
                else
                    if (prems) then
                        prems=.false.
!
                        if (nomgd .eq. 'DEPL_R') then
                            nomgd = 'DEPL_C'
                        endif
!
                        call cnocre(mailla, nomgd, nbnoeu, zi(inumno), ncmp,&
                                    zk8(inocmp), zi(inoecp), 'G', ' ', chamno)
                        call dismoi('PROF_CHNO', chamno, 'CHAM_NO', repk=prof)
                    else
                        call cnocre(mailla, nomgd, nbnoeu, zi( inumno), ncmp,&
                                    zk8(inocmp), zi(inoecp), 'G', prof, chamno)
                    endif
                endif
            else
                ASSERT(.false.)
            endif
            chamno(20:24) = '.VALE'
            call jeexin(chamno, ibid)
            if (ibid .gt. 0) then
                chamno(20:24) = '.VALE'
            else
                chamno(20:24) = '.CELV'
            endif
            call jeveuo(chamno, 'E', lvale)
            call jelira(chamno, 'TYPE', cval=type1)
            ASSERT(type1.eq.'C')
!
            if (leffor .or. .not.tousno) call jelira(chamno, 'LONMAX', neq)
!             IF (INTERP(1:3).NE.'NON') THEN
!               CALL EXTRAC(INTERP,EPSI,CRIT,NBINSG,ZR(IDINSG),
!     &               ZR(JFREQ+I),ZC(IDRESU),NBMODE,ZR(IDVECG), IBID)
!               CALL MDGPHC(NEQ,NBMODE,ZR(IDBASE),ZC(IDVECG),ZC(LVALE))
!             ELSE
            call mdgepc(neq, nbmode, base, zc(idresu+(zi(jnume+i)- 1)*nbmode), zc(lvale))
!             ENDIF
!
            call rsnoch(nomres, type(ich), iarchi)
            call rsadpa(nomres, 'E', 1, 'FREQ', iarchi,&
                        0, sjv=lfreq, styp=k8b)
            zr(lfreq) = zr(jfreq+i)
        end do
        AS_DEALLOCATE(vr=base)
    end do
!
    if (mode .eq. ' ') call refdcp(basemo, nomres)
!
!
! --- MENAGE
!
    call jedetr('&&HARM75.NUME_NOEUD  ')
    call jedetr('&&HARM75.NOM_CMP     ')
    call jedetr('&&HARM75.NB_NEQ      ')
    call jedetr('&&HARM75.NUME_DDL    ')
    call jedetr('&&HARM75.NUM_RANG')
    call jedetr('&&HARM75.FREQ')
    call jedetr('&&HARM75.VECTGENE')
    call detrsd('CHAM_NO', '&&HARM75.CHAMNO')
!
    call titre()
!
    call jedema()
end subroutine
