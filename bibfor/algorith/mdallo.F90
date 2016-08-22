subroutine mdallo(nomres, typcal, nbsauv, base, nbmodes,&
                  rigi, mass, amor, jordr, jdisc,&
                  nbsym, nomsym, jdepl, jvite, jacce,&
                  method, dt, jptem, nbnli, sd_nl_,&
                  jvint, sauve, checkarg)
! aslint: disable=W1504
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/crevec.h"
#include "asterfort/dismoi.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jeveut.h"
#include "asterfort/nlget.h"
#include "asterfort/mdtr74grd.h"
#include "asterfort/r8inir.h"
#include "asterfort/refdaj.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!   Obligatory arguments
    character(len=8), intent(in) :: nomres
    character(len=4), intent(in) :: typcal
    integer, intent(in) :: nbsauv
!   Optional arguments
    character(len=*), optional, intent(in) :: base
    integer, optional, intent(in) :: nbmodes
    character(len=*), optional, intent(in) :: rigi, mass, amor
    integer, optional, intent(out) :: jordr, jdisc
    integer, optional, intent(in) :: nbsym
    character(len=4), optional, intent(in) :: nomsym(*)
    integer, optional, intent(out) :: jdepl, jvite, jacce
    character(len=*), optional, intent(in) :: method
    real(kind=8), optional, intent(in) :: dt
    integer, optional, intent(out) :: jptem
    integer, optional, intent(in) :: nbnli
    character(len=*), optional, intent(in) :: sd_nl_
    integer, optional, intent(out) :: jvint
    character(len=4), optional, intent(in) :: sauve
    aster_logical , optional, intent(in) :: checkarg
! ----------------------------------------------------------------------
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
!     ALLOCATION DES VECTEURS DE SORTIE POUR UN CALCUL TRANSITOIRE
!     SUR BASE GENERALISEE (SD_DYNA_GENE)
!     ------------------------------------------------------------------
! IN  : NOMRES : NOM DU RESULTAT
! IN  : BASEMO : NOM DU CONCEPT BASE MODALE
! IN  : MASGEN : NOM DU CONCEPT MASSE GENERALISEE
! IN  : RIGGEN : NOM DU CONCEPT RAIDEUR GENERALISEE
! IN  : AMOGEN : NOM DU CONCEPT AMORTISSEMENT GENERALISE
! IN  : NBMODE : NOMBRE DE MODES
! IN  : DT     : PAS DE TEMPS
! IN  : NBPAS  : NOMBRE DE PAS CALCULE (INITIAL COMPRIS)
! IN  : NBNLI  : NOMBRE DE NON LINEARITES LOCALISEES
! IN  : INTITU : TABLEAU DES NOMS DES LIAISONS NONLINEAIRES (3 ENTREES PAR NON LINEARITE)
! IN  : METHOD : ALGORITHME UTILISE (DEVOGE, DIFF_CENTRE, ...)
!                DANS LE CAS ITMI, UN OBJET EST DIFFERENT
! IN  : TYPCAL : VAUT 'HARM' OU 'TRAN'
! IN  : SAUVE :  VAUT 'GLOB' OU 'VOLA'
! IN  : CHECKARG : VERIFIER LA COHERENCE DANS LES ARGUMENTS OPTIONNELS D'ENTREE
! ----------------------------------------------------------------------
    aster_logical :: checkargs, entvid, saved
    integer :: nbstoc, j1refe, inom, i, iret, jchmp, nbnoli, nbvint
    integer :: jdesc, jinti, nbsym2, nbmode, jnltyp, jvindx
    real(kind=8) :: dt2
    character(len=3) :: typsau
    character(len=4) :: sauve2, nomsym2(3)
    character(len=8) :: basemo, riggen, masgen, amogen, numgen, blanc, sd_nl
    character(len=5) :: attrib
    character(len=12) :: bl11pt
    character(len=16) :: method2
    character(len=24) :: matric(3)

    integer, pointer :: vindx(:) => null()
!-----------------------------------------------------------------------
!   --- 0 - Obligatory arguments, validation of the input values
    ASSERT((typcal.eq.'TRAN').or.(typcal.eq.'HARM'))
    ASSERT(nbsauv.ge.0)

!
!   --- Default values of input arguments
    basemo = ' '
    nbmode = 0
    riggen = ' '
    masgen = ' '
    amogen = ' '
    nbsym2 = 3
    nomsym2 = ['DEPL','VITE','ACCE']
    method2 = ' '
    dt2 = 0.0d0
    nbnoli = 0
    sauve2 = 'GLOB'
    checkargs = .true.
    sd_nl = sd_nl_
    if (present(base)) basemo = base
    if (present(nbmodes)) nbmode = nbmodes
    if (present(rigi)) riggen = rigi
    if (present(mass)) masgen = mass
    if (present(amor)) amogen = amor
    if (present(method)) method2 = method
    if (present(dt)) dt2 = dt
    if (present(nbnli )) nbnoli = nbnli 
    if (present(sauve)) sauve2 = sauve
    if (present(checkarg)) checkargs = checkarg
    if (present(nbsym) .and. present(nomsym)) then
        nbsym2 = nbsym
        do inom = 1, nbsym2
            nomsym2(inom) = nomsym(inom)
        end do
    endif
!
!   --- Initialize all output jeveux pointers to 1 
    if (present(jdepl)) jdepl = 1
    if (present(jvite)) jvite = 1
    if (present(jacce)) jacce = 1
    if (present(jptem)) jptem = 1
    if (present(jordr)) jordr = 1
    if (present(jdisc)) jdisc = 1
    if (present(jvint)) jvint = 1
!
!   --- If checkarg is .true. then verify the coherence of all optional arguments, 
    if (checkargs) then
!       --- 1 - Coherence between the modal basis and the number of modes
        ASSERT(AU_MOINS_UN2(base,nbmodes))
        if (present(base)) then
            if (base .ne. ' ') call dismoi('NB_MODES_TOT', base, 'RESULTAT', repi=nbmode)
        endif
        if (present(nbmodes)) then
            ASSERT((nbmode.eq.0).or.(nbmode.eq.nbmodes))
            nbmode = nbmodes
        endif
!       --- 2 - If nbsauv = 0, then only initialize some objects => no vector retrieval
        if (nbsauv .eq. 0) then
            ASSERT((absent(jordr)).and.(absent(jdisc)))
            ASSERT((absent(jdepl)).and.(absent(jvite)).and.(absent(jacce)))
            ASSERT(absent(jptem))
!       --- 3 - If nbsauv != 0
        else
!           --- 3.1 - Get the symbolic names of the saved fields or use def vals
            ASSERT(ENSEMBLE2(nbsym,nomsym))
            if (typcal .eq. 'TRAN') then
                ASSERT(absent(nbsym))
            endif
!           --- 3.2 - Verify according to nomsym if it possible to retrieve the 
!                     displacement, velocity, and acceleration vectors.
            saved = .false.
            do inom = 1, nbsym2
                if (nomsym2(inom) .eq. 'DEPL') saved = .true.
            end do
            if (.not.(saved)) then
                ASSERT(absent(jdepl))
            endif
            saved = .false.
            do inom = 1, nbsym2
                if (nomsym2(inom) .eq. 'VITE') saved = .true.
            end do
            if (.not.(saved)) then
                ASSERT(absent(jvite))
            endif
            saved = .false.
            do inom = 1, nbsym2
                if (nomsym2(inom) .eq. 'ACCE') saved = .true.
            end do
            if (.not.(saved)) then
                ASSERT(absent(jacce))
            endif
!           --- 3.3 - No time step or integration method are allowed in harmonic case
            if (typcal .eq. 'HARM') then
                ASSERT(absent(dt))
                ASSERT(absent(method))
                ASSERT(absent(jptem))
            endif
        endif
!       --- 4 - Treatment of choc parameters/arguments
        if (nbnoli .ne. 0) then
            ASSERT(present(sd_nl_))
        endif

!       --- 5 - Treatment of sauv parameter, global (default) or volatile saving
        ASSERT((sauve2(1:4).eq.'GLOB'.or.sauve2(1:4).eq.'VOLA'))
!   --- End of argument verification
    endif
!
    if (sauve2(1:4) .eq. 'GLOB') typsau='G V'
    if (sauve2(1:4) .eq. 'VOLA') typsau='V V'
    nbstoc = nbmode * nbsauv
!
    jchmp = 1
    jdesc = 1
    blanc = '        '
    bl11pt = '           .'
!
    call jeexin(nomres//'           .REFD', iret)
    entvid = .false.
    if ((riggen .eq. ' ') .and. (masgen .eq. ' ') .and. (amogen .eq. ' ')) entvid = .true.
!
    if (iret .eq. 0) then
        if (entvid) then
            if (basemo .ne. blanc) then
                matric(1) = basemo
                call refdaj('F', nomres, nbsauv, ' ', 'MESURE',&
                            matric, iret)
            else
                call refdaj(' ', nomres, nbsauv, ' ', 'INIT',&
                            ' ', iret)
            endif
        else
!           On recupere la numerotation generalisee
            call jeexin(riggen(1:8)//'           .REFA', iret)
            if (iret .ne. 0) then
                call jeveuo(riggen(1:8)//'           .REFA', 'L', j1refe)
                numgen = zk24(j1refe+1)(1:8)
            else
                numgen = blanc
            endif
            matric(1) = riggen(1:8)
            matric(2) = masgen(1:8)
            matric(3) = amogen(1:8)
            call refdaj('F', nomres, nbsauv, numgen(1:8), 'DYNAMIQUE',&
                        matric, iret)
        endif
    endif
!
!   .DESC is already created in mdrecf (external forces retrieval), and DESC[5] is set
!   to 1 if a static correction is considered.
    call jeexin(nomres//'           .DESC', iret)
    if (iret .eq. 0) then
        call wkvect(nomres//'           .DESC', typsau//' I', 5, jdesc)
        do i = 1, 5
            zi(jdesc+i-1) = 0
        end do
    else 
        call jeveuo(nomres//'           .DESC', 'E', jdesc)
    end if
    zi(jdesc) = 1
!
    if (typcal .eq. 'HARM') then
        zi(jdesc) = 4
!      -- DANS LE CAS 'HARM' ON REMPLIT LA VALEUR A 4
!
!      -- BLINDAGE : VERIFICATION DE NBSYM ET NOMSYM
        if ((nbsym2.le.0) .or. (nbsym2.ge.4)) then
            call utmess('F', 'ALGORITH17_29')
        endif
        do inom = 1, nbsym2
            if ((nomsym2(inom)(1:4).ne.'DEPL') .and. (nomsym2(inom)( 1:4).ne.'VITE') .and.&
                (nomsym2(inom)(1:4).ne.'ACCE')) then
                call utmess('F', 'ALGORITH17_29')
            endif
        enddo
    else if (typcal.eq.'TRAN') then
!     -- INITIALISATION DES CHAMPS A ALLOUER DANS LE CAS TRANS.
        nbsym2 = 3
        nomsym2(1) = 'DEPL'
        nomsym2(2) = 'VITE'
        nomsym2(3) = 'ACCE'
        if (nbnoli .ne. 0) then
            zi(jdesc) = 2
        endif
!     -DANS LE CAS ITMI ET ADAPT (METHODES A PAS VARIABLE),
!      ON MET LA VALEUR 3 QUI SERVIRA DE TEST
!       A LA COMMANDE POST_DYNA_MODA_T
        if ((method2(1:4) .ne. 'DIFF') .and. (method2(1:7) .ne. 'NEWMARK')) then
            zi(jdesc) = 3
        endif
!     DANS LE CAS TRANSITOIRE, ON REMPLIT TOUJOURS LES TROIS CHAMPS
    endif
    zi(jdesc+1) = nbmode
    zi(jdesc+2) = nbnoli
    zi(jdesc+3) = 0

!
    if (typcal .eq. 'TRAN') then
        attrib = typsau//' R'
        nbsym2 = 3
        nomsym2(1) = 'DEPL'
        nomsym2(2) = 'VITE'
        nomsym2(3) = 'ACCE'
    else
        attrib = typsau//' C'
    endif
!
    if (nbsauv .ne. 0) then
!       BOUCLE SUR LES CHAMPS A SAUVEGARDER (DEPL/VITE/ACCE)
        do inom = 1, nbsym2
            call crevec(nomres//bl11pt//nomsym2(inom), attrib, nbstoc, jchmp)
!           INITIALISATION DES CHAMPS A ZERO
            if (typcal .eq. 'TRAN') then
                call r8inir(nbstoc, 0.d0, zr(jchmp), 1)
            else
                do i = 0, nbstoc-1
                    zc(jchmp+i) = dcmplx(0.d0,0.d0)
                enddo
            endif
            if ((nomsym2(inom) .eq. 'DEPL').and.(present(jdepl))) jdepl=jchmp
            if ((nomsym2(inom) .eq. 'VITE').and.(present(jvite))) jvite=jchmp
            if ((nomsym2(inom) .eq. 'ACCE').and.(present(jacce))) jacce=jchmp
        enddo
!
!       OBJETS COMMUNS
        call crevec(nomres//'           .ORDR', typsau//' I', nbsauv, jordr)
        call crevec(nomres//'           .DISC', typsau//' R', nbsauv, jdisc)
        if (typcal .eq. 'TRAN') then
            call crevec(nomres//'           .PTEM', typsau//' R', nbsauv, jptem)
            zr(jptem) = dt
        endif
    endif
!
!   CREATION DES VECTEURS DE STOCKAGE DES FORCES DE CHOC
    if (nbnoli .ne. 0) then
        call nlget(sd_nl, _INTERNAL_VARS_INDEX, vi=vindx)
        nbvint = vindx(nbnoli+1)-1
        zi(jdesc+3) = nbvint

        if (nbvint*nbsauv .ne. 0) then
!           Index
            call crevec(nomres//'        .NL.VIND', typsau//' I', nbnoli+1, jvindx)
            call nlget(sd_nl, _INTERNAL_VARS_INDEX, ivect=zi(jvindx))
!           Internal variables object
            call crevec(nomres//'        .NL.VINT', typsau//' R', nbvint*nbsauv, jvint)
            call r8inir(nbvint*nbsauv, 0.d0, zr(jvint), 1)
        endif

        call jeexin(nomres//'        .NL.TYPE', iret)
        if (iret .eq. 0) then
            call crevec(nomres//'        .NL.TYPE', typsau//' I', nbnoli, jnltyp)
            do i = 1, nbnoli
                call nlget(sd_nl, _NL_TYPE, iocc=i, iscal=zi(jnltyp-1+i))                
            end do
        endif

        call jeexin(nomres//'        .NL.INTI', iret)
        if (iret .eq. 0) then
            call crevec(nomres//'        .NL.INTI', typsau//' K24', nbnoli*5, jinti)
            do i = 1, 4*nbnoli
                zk24(jinti-1+i) = ' '
            end do
            do i = 1, nbnoli
                call nlget(sd_nl, _NL_TITLE, iocc=i, lonvec=iret)
                if (iret.ne.0) call nlget(sd_nl, _NL_TITLE, iocc=i, kscal=zk24(jinti-1+(i-1)*5+1))
                
                call nlget(sd_nl, _NO1_NAME, iocc=i, lonvec=iret)
                if (iret.ne.0) call nlget(sd_nl, _NO1_NAME, iocc=i, kscal=zk24(jinti-1+(i-1)*5+2))

                call nlget(sd_nl, _NO2_NAME, iocc=i, lonvec=iret)
                if (iret.ne.0) call nlget(sd_nl, _NO2_NAME, iocc=i, kscal=zk24(jinti-1+(i-1)*5+3))

                call nlget(sd_nl, _SS1_NAME, iocc=i, lonvec=iret)
                if (iret.ne.0) call nlget(sd_nl, _SS1_NAME, iocc=i, kscal=zk24(jinti-1+(i-1)*5+4))

                call nlget(sd_nl, _SS2_NAME, iocc=i, lonvec=iret)
                if (iret.ne.0) call nlget(sd_nl, _SS2_NAME, iocc=i, kscal=zk24(jinti-1+(i-1)*5+5))
            enddo
        endif
    endif
!
end subroutine
