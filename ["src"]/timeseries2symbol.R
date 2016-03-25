function(data, n, alphabet_size) {
        
        # This function transforms a time series subsequence to a symbolic representation
        #
        # Input:
        #   data              is the raw time series as a numerical vector. Practically, it is
        #                     meaningless to convert the whole original time series into a SAX
        #                     string. Instead, we need to chunk it into subsequences first and
        #                     then apply function 'timeseries2Symbol' to each subsequence. So
        #                     here data is a subsequence to be symbolized. See 'timeseries2Subseqs.r'
        #                     upon how to transform original time series into subsequences.
        #
        #   n                 is the number of symbols in the low dimensional approximation of 
        #                     the sub sequence, i.e., the word size
        #
        #   alphabet_size     is the number of discrete symbols. 2 <= alphabet_size <= 20, although 
        #                     alphabet_size = 2 is a special "useless" case.
        #
        # Output:
        #   sax_rep:  a list of two elements, i.e. (list(num_rep = num_rep, str_rep = str_rep)). 
        #             sax_rep[[1]] or sax_rep$num_rep is the numerical representation of SAX
        #             string as an integer vector, sax_rep[[2]] or sax_rep$str_rep is the string 
        #             representation of SAX as a string
        #
        # The variable "win_size" is assigned to N/n, where N = lenght(data). This is the number 
        # of data points on the raw time series that will be mapped to a single symbol, and can 
        # be imagined as the "compression rate".
        #
        # For details, see below papers
        #
        #   Lin, J., Keogh, E., Lonardi, S. & Chiu, B. 
        #   "A Symbolic Representation of Time Series, with Implications for Streaming Algorithms." 
        #   In proceedings of the 8th ACM SIGMOD Workshop on Research Issues in Data Mining and 
        #   Knowledge Discovery. San Diego, CA. June 13, 2003. 
        #
        #   Lin, J., Keogh, E., Patel, P. & Lonardi, S. 
        #   "Finding Motifs in Time Series". In proceedings of the 2nd Workshop on Temporal Data Mining, 
        #   at the 8th ACM SIGKDD International Conference on Knowledge Discovery and Data Mining. 
        #   Edmonton, Alberta, Canada. July 23-26, 2002  
        
        if (alphabet_size > 20) {
                stop('Currently alphabet_size cannot be larger than 20. Please update the breakpoint table if you wish to do so')
        }
        
        # length of the time series
        N = length(data)
        
        # win_size is the number of data points on the raw time series that will be
        # mapped to a single symbol
        win_size = floor(N/n)                         
        
        # Before normalizing, check for variability in subsquence. If none, normalization will 
        # return NaN when dividing by 0, sd(data).
        if(var(data) == 0){
                data = rep.int(0, length(d))
        } else {
        # Z normalize it, as it is meaningless to compare ts with different offsets and amplitudes
                data = (data - mean(data)) / sd(data)  
        }
        
        if (N <= n){
                # take care of the special case where there is no dimensionality reduction needed
                PAA = data
        }
        else {
                # Convert to PAA.
                
                # N is not dividable by n
                if ((N %% n) != 0) {                               
                        temp = matrix(0, nrow = n, ncol = N)
                        
                        for (i in 1 : n){
                                temp[i, ] = data
                        }
                        
                        expanded_data = matrix(temp, 1, N * n)
                        
                        PAA = colMeans(matrix(expanded_data, N, n), na.rm = TRUE)
                        
                } else {
                        # N is dividable by n
                        PAA = colMeans(matrix(data, win_size, n), na.rm = TRUE)
                }
        }
        
        # Convert the PAA to SAX representation
        sax_rep = paa2SAX(PAA, alphabet_size)        
        
        return(sax_rep)                                         
}
